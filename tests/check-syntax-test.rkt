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

;; ============================================================
;; check-syntax-analyze
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
