#lang racket/base

(require
  racket/port
  racket/contract/base
  racket/syntax
  racket/list
  racket/string
  racket/set
  racket/hash
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide module/require-provide)

(define-rule module/require-provide
  #:id 'module/require-provide
  #:severity 'warning
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
    (for/list ([prov (in-list provided)])
      (define id (second prov))
      (diagnostic path (or (syntax-line id) 1) (or (syntax-column id) 1)
                  'info 'module/require-provide
                  (format "Module provides ~a" (first prov))))))
