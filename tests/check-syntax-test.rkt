#lang racket/base

(require rackunit
         racket/port
         racket/file
         racket/path
         "../core/check-syntax.rkt"
         "../core/diagnostic.rkt")

(define (make-temp content)
  (define f (make-temporary-file "test-~a.rkt"))
  (display-to-file content f #:exists 'replace)
  f)

(test-case "check-syntax: facts retain definitions and references"
  (define f (make-temp "#lang racket/base\n(define x 1)\n(displayln x)\n"))
  (define facts (check-syntax-facts (path->string f)))
  (check-equal? (length (syntax-facts-errors facts)) 0)
  (check-true (pair? (syntax-facts-definitions facts)))
  (check-true (pair? (syntax-facts-references facts)))
  (check-equal? (length (syntax-facts-unused-binders facts)) 0)
  (delete-file f))

(test-case "check-syntax: unused span maps to source line"
  (define f (make-temp "#lang racket/base\n(define unused 1)\n(displayln 2)\n"))
  (define facts (check-syntax-facts (path->string f)))
  (define span (car (syntax-facts-unused-binders facts)))
  (check-equal? (syntax-span-start span) 26)
  (define diags (check-syntax-analyze (path->string f)))
  (define unused (car diags))
  (check-equal? (diagnostic-line unused) 2)
  (check-equal? (diagnostic-col unused) 8)
  (delete-file f))

(test-case "check-syntax: unused require has a source span"
  (define f (make-temp "#lang racket/base\n(require racket/string)\n(displayln 1)\n"))
  (define facts (check-syntax-facts (path->string f)))
  (check-equal? (length (syntax-facts-unused-requires facts)) 1)
  (define span (car (syntax-facts-unused-requires facts)))
  (check-true (<= 0 (syntax-span-start span)))
  (delete-file f))


;; ============================================================

(test-case "check-syntax: returns list"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (check-true (list? (check-syntax-analyze (path->string f))))
  (delete-file f))

(test-case "check-syntax: detects unused variable"
  (define f (make-temp "#lang racket/base\n(define x 1)\n(define y 2)\n(displayln x)\n"))
  (define diags (check-syntax-analyze (path->string f)))
  (check-true (>= (length diags) 1))
  (delete-file f))

(test-case "check-syntax: no diagnostics for used variable"
  (define f (make-temp "#lang racket/base\n(define x 1)\n(displayln x)\n"))
  (define diags (check-syntax-analyze (path->string f)))
  ;; x is used, so no unused variable diagnostic
  (define unused-vars (filter (lambda (d) (eq? (diagnostic-rule-id d) 'check-syntax/unused-variable)) diags))
  (check-equal? (length unused-vars) 0)
  (delete-file f))

(test-case "check-syntax: diagnostics have correct rule-id"
  (define f (make-temp "#lang racket/base\n(define x 1)\n(define y 2)\n(displayln x)\n"))
  (define diags (check-syntax-analyze (path->string f)))
  (for ([d (in-list diags)])
    (define rid (diagnostic-rule-id d))
    (check-true (or (eq? rid 'check-syntax/unused-variable)
                    (eq? rid 'check-syntax/unused-require))))
  (delete-file f))

(test-case "check-syntax: diagnostics have path"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (define diags (check-syntax-analyze (path->string f)))
  (for ([d (in-list diags)])
    (check-equal? (diagnostic-path d) (path->string f)))
  (delete-file f))
