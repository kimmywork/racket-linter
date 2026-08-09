#lang racket/base

(require
  racket/contract/base
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  "../../core/check-syntax.rkt"
  (for-syntax racket/base))

(provide check-syntax/unused)

;; Rule that uses DrRacket's check-syntax for unused variable/require detection
(define-rule check-syntax/unused
  #:id 'check-syntax/unused
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (check-syntax-analyze path)))
