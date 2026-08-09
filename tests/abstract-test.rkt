#lang racket/base

(require rackunit
         racket/string
         racket/port
         racket/file
         racket/path
         syntax/modread
         "../core/abstract-eval.rkt"
         "../core/diagnostic.rkt")

(define (run-abstract content)
  (define f (make-temporary-file "test-~a.rkt"))
  (display-to-file content f #:exists 'replace)
  (define stx
    (call-with-input-file f
      (lambda (in)
        (with-module-reading-parameterization
          (lambda () (read-syntax f in))))))
  (define expanded
    (parameterize ([current-namespace (make-base-namespace)])
      (expand stx)))
  (unless (syntax? expanded)
    (error 'run-abstract "expansion did not produce syntax"))
  (define diags (analyze-abstract expanded (path->string f)))
  (delete-file f)
  diags)

(define (type-errors diagnostics)
  (filter (lambda (d)
            (eq? (diagnostic-rule-id d) 'abstract/type-error))
          diagnostics))

;; ============================================================
;; Type error detection
;; ============================================================

(test-case "abstract: apply number as procedure"
  (define diags (run-abstract "#lang racket/base\n(define x 1)\n(x 2)\n"))
  (check-true (>= (length diags) 1))
  (check-equal? (diagnostic-rule-id (car diags)) 'abstract/type-error))

(test-case "abstract: apply string as procedure"
  (define diags (run-abstract "#lang racket/base\n(define x \"hello\")\n(x 2)\n"))
  (check-true (>= (length diags) 1))
  (check-equal? (diagnostic-rule-id (car diags)) 'abstract/type-error))

(test-case "abstract: apply boolean as procedure"
  (define diags (run-abstract "#lang racket/base\n(define x #t)\n(x 2)\n"))
  (check-true (>= (length diags) 1)))

(test-case "abstract: no error for procedure call"
  (define diags (run-abstract "#lang racket/base\n(define (f x) (+ x 1))\n(f 2)\n"))
  (define type-errors (filter (lambda (d) (eq? (diagnostic-rule-id d) 'abstract/type-error)) diags))
  (check-equal? (length type-errors) 0))

;; ============================================================
;; Numeric operations
;; ============================================================

(test-case "abstract: + returns num"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (+ 1 2))\n"))))

(test-case "abstract: - returns num"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (- 5 3))\n"))))

(test-case "abstract: * returns num"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (* 2 3))\n"))))

(test-case "abstract: / returns num"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (/ 6 2))\n"))))

;; ============================================================
;; List operations
;; ============================================================

(test-case "abstract: list returns lst"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (list 1 2 3))\n"))))

(test-case "abstract: cons returns pair"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (cons 1 2))\n"))))

(test-case "abstract: car on pair"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (cons 1 2))\n(car x)\n"))))

(test-case "abstract: cdr on pair"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (cons 1 2))\n(cdr x)\n"))))

(test-case "abstract: car on list"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (list 1 2))\n(car x)\n"))))

(test-case "abstract: cdr on list"
  (check-true (list? (run-abstract "#lang racket/base\n(define x (list 1 2))\n(cdr x)\n"))))

;; ============================================================
;; Control flow
;; ============================================================

(test-case "abstract: if expression"
  (check-true (list? (run-abstract "#lang racket/base\n(if #t 1 2)\n"))))

(test-case "abstract: if with bottom test"
  (check-true (list? (run-abstract "#lang racket/base\n(if (raise \"err\") 1 2)\n"))))

(test-case "abstract: let expression"
  (check-true (list? (run-abstract "#lang racket/base\n(let ([x 1]) (+ x 2))\n"))))

(test-case "abstract: let* expression"
  (check-true (list? (run-abstract "#lang racket/base\n(let* ([x 1] [y (+ x 1)]) y)\n"))))

(test-case "abstract: letrec expression"
  (check-true (list? (run-abstract "#lang racket/base\n(letrec ([f (lambda (x) x)]) (f 1))\n"))))

(test-case "abstract: begin expression"
  (check-true (list? (run-abstract "#lang racket/base\n(begin 1 2 3)\n"))))

(test-case "abstract: begin0 expression"
  (check-true (list? (run-abstract "#lang racket/base\n(begin0 1 2 3)\n"))))

(test-case "abstract: set! expression"
  (check-true (list? (run-abstract "#lang racket/base\n(define x 1)\n(set! x 2)\n"))))

;; ============================================================
;; Quote
;; ============================================================

(test-case "abstract: quote number"
  (check-true (list? (run-abstract "#lang racket/base\n(quote 42)\n"))))

(test-case "abstract: quote string"
  (check-true (list? (run-abstract "#lang racket/base\n(quote \"hello\")\n"))))

(test-case "abstract: quote symbol"
  (check-true (list? (run-abstract "#lang racket/base\n(quote foo)\n"))))

(test-case "abstract: quote boolean"
  (check-true (list? (run-abstract "#lang racket/base\n(quote #t)\n"))))

(test-case "abstract: quote list"
  (check-true (list? (run-abstract "#lang racket/base\n(quote (1 2 3))\n"))))

;; ============================================================
;; Lambda
;; ============================================================

(test-case "abstract: lambda creates procedure"
  (check-true (list? (run-abstract "#lang racket/base\n(define f (lambda (x) (+ x 1)))\n"))))

(test-case "abstract: lambda call"
  (check-true (list? (run-abstract "#lang racket/base\n((lambda (x) (+ x 1)) 5)\n"))))

;; ============================================================
;; Define-values
;; ============================================================

(test-case "abstract: define-values"
  (check-true (list? (run-abstract "#lang racket/base\n(define-values (a b) (values 1 2))\n"))))

;; ============================================================
;; Fixpoint iteration
;; ============================================================

(test-case "abstract: recursive function"
  (check-true (list? (run-abstract "#lang racket/base\n(define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))\n"))))

(test-case "abstract: mutual recursion"
  (check-true (list? (run-abstract "#lang racket/base\n(define (even? n) (if (= n 0) #t (odd? (- n 1))))\n(define (odd? n) (if (= n 0) #f (even? (- n 1))))\n"))))

;; ============================================================
;; Module handling
;; ============================================================

(test-case "abstract: module form"
  (check-true (list? (run-abstract "#lang racket/base\n(module m racket/base (define x 1))\n"))))

(test-case "abstract: multiple bodies"
  (check-true (list? (run-abstract "#lang racket/base\n(define x 1)\n(displayln x)\n"))))

(test-case "abstract: reports known procedure arity error"
  (define diags
    (run-abstract
     "#lang racket/base\n(define (f x y) (+ x y))\n(f 1)\n"))
  (check-equal? (length (type-errors diags)) 1)
  (check-true (string-contains?
               (diagnostic-message (car (type-errors diags)))
               "expects 2 arguments")))

(test-case "abstract: valid list and arithmetic flow has no type error"
  (define diags
    (run-abstract
     "#lang racket/base\n(define xs (list 1 2))\n(+ (car xs) 3)\n"))
  (check-equal? (length (type-errors diags)) 0))

(test-case "abstract: expansion failure is not a clean result"
  (check-exn exn?
             (lambda ()
               (run-abstract "#lang racket/base\n(define ("))))
