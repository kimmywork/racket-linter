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
           [(symbol? e) (list stx)]
           [(pair? e)
            (append (collect-identifiers (car stx))
                    (apply append (map collect-identifiers (cdr stx))))]
           [else '()])]))
    
    ;; Get all identifiers from the expanded syntax
    (define all-identifiers (collect-identifiers stx))
    
    ;; Filter to only identifiers that have binding information
    (define bound-identifiers
      (filter (lambda (id)
                (with-handlers ([exn? (lambda (e) #f)])
                  (identifier-binding id)))
              all-identifiers))
    
    ;; Group identifiers by their source module
    (define by-module (make-hash))
    (for ([id (in-list bound-identifiers)])
      (define binding (with-handlers ([exn? (lambda (e) #f)]) (identifier-binding id)))
      (when (and binding (list? binding) (>= (length binding) 2))
        (define mod-path (second binding))
        (when mod-path
          (hash-update! by-module mod-path
                        (lambda (old) (cons id old))
                        '()))))
    
    ;; For each module, check if any of its bindings are used
    ;; If a module has no used bindings, it's likely an unused require
    (define unused-modules '())
    (for ([(mod-path ids) (in-hash by-module)])
      (when (null? ids)
        (set! unused-modules (cons mod-path unused-modules))))
    
    ;; Generate diagnostics for unused modules
    (for/list ([mod (in-list unused-modules)])
      (diagnostic path 1 1 'info 'reachability/unused-require-expand
                  (format "Module ~a may be unused (no bindings found in expanded code)" mod)))))
