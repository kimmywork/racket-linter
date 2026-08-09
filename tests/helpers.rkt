#lang racket/base

(require racket/port
         racket/file
         racket/path
         syntax/modread
         "../core/rule.rkt"
         "../core/engine.rkt")

(provide run-rule-on run-syntax-rule make-temp-rkt run-abstract-on project-root)

;; Project root for absolute path resolution
(define project-root
  (path->string (path-only (path->string (collection-file-path "lint.rkt" "racket-linter" "cli")))))

(define (run-rule-on rule content)
  (define temp-file (make-temporary-file "test-~a.rkt"))
  (call-with-output-file temp-file
    (lambda (out) (display content out))
    #:exists 'replace)
  (define diags ((rule-check rule) #f (path->string temp-file) (hash)))
  (delete-file temp-file)
  diags)

(define (run-syntax-rule rules content)
  (define temp-file (make-temporary-file "test-~a.rkt"))
  (display-to-file (string-append "#lang racket/base\n" content "\n") temp-file #:exists 'replace)
  (define config
    (for/hash ([rule (in-list rules)])
      (values (rule-id rule) (hash 'enabled #t))))
  (define diags (run-file rules config (path->string temp-file)))
  (delete-file temp-file)
  diags)

(define (make-temp-rkt content)
  (define temp-file (make-temporary-file "test-~a.rkt"))
  (display-to-file content temp-file #:exists 'replace)
  temp-file)

(define (run-abstract-on content)
  (define temp-file (make-temp-rkt content))
  (define stx
    (call-with-input-file temp-file
      (lambda (in)
        (with-module-reading-parameterization
          (lambda () (read-syntax temp-file in))))))
  (define diags (if (syntax? stx)
                    ((dynamic-require "../core/abstract-eval.rkt" 'analyze-abstract) stx (path->string temp-file))
                    '()))
  (delete-file temp-file)
  diags)
