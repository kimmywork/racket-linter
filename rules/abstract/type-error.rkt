#lang racket/base

(require
  racket/port
  racket/contract/base
  racket/syntax
  racket/list
  racket/string
  racket/set
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  "../../core/abstract-eval.rkt"
  (for-syntax racket/base))

(provide abstract/type-error)

;; Abstract interpretation-based type error detection
;; This rule uses abstract interpretation to detect:
;; - Applying non-procedure values
;; - Unreachable branches

(define-rule abstract/type-error
  #:id 'abstract/type-error
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'expand
  (lambda (stx path config)
    (analyze-abstract stx path)))
