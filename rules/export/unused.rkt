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

(provide export/unused)

(define-rule export/unused
  #:id 'export/unused
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (define (get-provided-ids stx)
      (cond
        [(not (syntax? stx)) '()]
        [else
         (define e (syntax-e stx))
         (cond
           [(and (pair? e) (eq? (syntax-e (car e)) 'provide))
            (for/fold ([ids '()]) ([p (in-list (cdr e))])
              (define p-e (syntax-e p))
              (cond
                [(symbol? p-e)
                 (cons (list p-e p) ids)]
                [(and (pair? p-e) (memq (car p-e) '(struct-out only-in rename-in)))
                 (append ids (get-provided-ids p))]
                [else ids]))]
           [(pair? e)
            (apply append (map get-provided-ids (syntax-e stx)))]
           [else '()])]))
    (define provided (get-provided-ids stx))
    (when (null? provided)
      '())
    (define provided-names (map first provided))
    (define (collect-references stx)
      (cond
        [(not (syntax? stx)) '()]
        [else
         (define e (syntax-e stx))
         (cond
           [(identifier? stx)
            (if (memq (syntax-e stx) provided-names) '() (list stx))]
           [(pair? e)
            (append (collect-references (car e))
                    (apply append (map collect-references (cdr e))))]
           [else '()])]))
    (define all-references (collect-references stx))
    (define referenced-names (map syntax-e all-references))
    (define used-names (list->set referenced-names))
    (for/list ([prov (in-list provided)]
               #:when (not (set-member? used-names (first prov))))
      (define id (second prov))
      (diagnostic path (or (syntax-line id) 1) (or (syntax-column id) 1)
                  'info 'export/unused
                  (format "Exported identifier ~a is not used within the module" (first prov))))))
