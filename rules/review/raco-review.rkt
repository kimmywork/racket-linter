#lang racket/base

;; Optional bridge to the installed raco-review implementation. The native
;; review/syntax-quality rule covers stable local checks; this bridge preserves
;; complete compatibility with the review package's version-specific rules.

(require
  racket/contract/base
  racket/list
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt")

(provide review/raco-review)

(define (review-level level)
  (case level
    [(error) 'error]
    [(warning) 'warning]
    [else 'info]))

(define (run-review path)
  (define lint-proc (dynamic-require 'review/lint 'lint))
  (define problem-loc-proc (dynamic-require 'review/problem 'problem-loc))
  (define problem-level-proc (dynamic-require 'review/problem 'problem-level))
  (define problem-message-proc (dynamic-require 'review/problem 'problem-message))
  (define current-problems (dynamic-require 'review/problem 'current-problem-list))
  (parameterize ([current-problems '()])
    (for/list ([problem (in-list (lint-proc path))])
      (define-values (_source line column) (problem-loc-proc problem))
      (diagnostic path
                  (or line 1)
                  (or column 0)
                  (review-level (problem-level-proc problem))
                  'review/raco-review
                  (problem-message-proc problem)))))

(define-rule review/raco-review
  #:id 'review/raco-review
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (list
                        (diagnostic path 1 0 'error 'review/backend-error
                                    (format "raco review compatibility backend failed: ~a"
                                            (exn-message exn)))))])
      (run-review path))))
