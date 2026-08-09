#lang racket/base

(require
  (for-syntax racket/base))

(struct rule (id severity config-keys check layer)
  #:transparent)

;; Layer values:
;; 'text   - rule runs on raw text (stx argument is #f)
;; 'syntax - rule runs on parsed syntax object
;; 'both   - rule runs in BOTH text and syntax phases
;; Default (when #:layer is omitted) is 'syntax.

(define-syntax (define-rule stx)
  (syntax-case stx ()
    [(_ name
         #:id id-expr
         #:severity severity-expr
         #:config-keys keys-expr
         #:layer layer-expr
         check-expr)
     #'(define name
         (rule id-expr severity-expr keys-expr check-expr layer-expr))]
    [(_ name
         #:id id-expr
         #:severity severity-expr
         #:config-keys keys-expr
         check-expr)
     #'(define name
         (rule id-expr severity-expr keys-expr check-expr 'syntax))]))

(provide
  (struct-out rule)
  rule
  define-rule)
