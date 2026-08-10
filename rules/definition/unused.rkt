#lang racket/base

(require
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  "../../core/check-syntax.rkt")

(provide definition/unused)

;; Compatibility rule backed by check-syntax lexical binding identity. It no
;; longer reports every textual `define`; only binders proven unused by the
;; expanded module are reported.
(define-rule definition/unused
  #:id 'definition/unused
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (for/list ([finding (in-list (check-syntax-analyze path))]
               #:when (eq? (diagnostic-rule-id finding)
                           'check-syntax/unused-variable))
      (struct-copy diagnostic finding
                   [rule-id 'definition/unused]
                   [message "definition is not referenced by this module"]))))
