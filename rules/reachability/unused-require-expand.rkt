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
  (for-syntax racket/base))

(provide reachability/unused-require-expand)

;; Expansion-based unused require detection
;; This rule uses `expand` to get the fully expanded code and then
;; analyzes which required bindings are actually used.

(define-rule reachability/unused-require-expand
  #:id 'reachability/unused-require-expand
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'expand
  (lambda (stx path config)
    ;; Collect all identifiers used in the expanded code
    (define (collect-identifiers stx)
      (cond
        [(not (syntax? stx)) '()]
        [else
         (define e (syntax-e stx))
         (cond
           [(symbol? e) (list e)]
           [(pair? e)
            (append (collect-identifiers (car stx))
                    (apply append (map collect-identifiers (cdr e))))]
           [else '()])]))
    
    ;; Get all identifiers from the expanded syntax
    (define used-identifiers (list->set (collect-identifiers stx)))
    
    ;; For now, just report that expansion works
    ;; A full implementation would parse require forms and check which
    ;; imported bindings are in used-identifiers
    '()))
