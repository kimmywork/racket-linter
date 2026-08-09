#lang racket/base

;; Rule module for racket-linter
;;
;; Defines the rule struct and the define-rule macro for creating linter rules.
;; Rules are the core building blocks of the linter - each rule analyzes code
;; and produces diagnostics.

(require
  (for-syntax racket/base)
  racket/contract/base)

;; Represents a linter rule.
;;
;; Fields:
;;   id - symbol identifying this rule (e.g., 'style/line-length)
;;   severity - default severity for diagnostics from this rule
;;   config-keys - hash of default configuration for this rule
;;   check - function that takes (stx path config) and returns list of diagnostics
;;   layer - when this rule runs: 'text, 'syntax, 'expand, or 'both
(struct rule (id severity config-keys check layer)
  #:transparent)

;; Layer system:
;; 'text   - rule runs on raw text (stx argument is #f), always executed
;; 'syntax - rule runs on parsed syntax object, only for safe #lang files
;; 'expand - rule runs on expanded syntax, only for safe #lang files
;; 'both   - rule runs in BOTH text and syntax phases
;; Default (when #:layer is omitted) is 'syntax.

;; Macro for defining linter rules.
;;
;; Usage:
;;   (define-rule my-rule
;;     #:id 'my-rule
;;     #:severity 'warning
;;     #:config-keys (hash 'enabled #t)
;;     #:layer 'text
;;     (lambda (stx path config)
;;       ;; Rule implementation
;;       '()))
;;
;; The #:layer keyword is optional and defaults to 'syntax.
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
