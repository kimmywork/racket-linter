#lang racket/base

(require
  racket/string
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  "../../core/check-syntax.rkt")

(provide reachability/undefined)

;; Expansion-backed undefined-identifier detection. A successful expansion is
;; evidence that the identifier resolved in the active lexical/module context;
;; no name whitelist is needed. Provide errors are left to the provide rule.
(define-rule reachability/undefined
  #:id 'reachability/undefined
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (for/list ([message (in-list (syntax-facts-errors (check-syntax-facts path)))]
               #:when (and (regexp-match? #px"unbound identifier" message)
                           (not (regexp-match? #px"provide" message))))
      (diagnostic path 1 0 'warning 'reachability/undefined message))))
