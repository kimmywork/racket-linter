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

(provide reachability/unused-require)

(define-rule reachability/unused-require
  #:id 'reachability/unused-require
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (define (get-require-bindings stx)
      (cond
        [(not (syntax? stx)) '()]
        [else
         (define e (syntax-e stx))
         (cond
           [(and (pair? e) (eq? (syntax-e (car e)) 'require))
            (define rest (cdr e))
            (for/fold ([acc '()]) ([req (in-list rest)])
              (define req-e (syntax-e req))
              (cond
                [(symbol? req-e)
                 (cons (list req-e req) acc)]
                [(and (pair? req-e) (eq? (car req-e) 'only-in))
                 (for/fold ([bs '()]) ([name (in-list (cddr req-e))])
                   (cons (list (syntax-e name) name) bs))]
                [(and (pair? req-e) (eq? (car req-e) 'except-in))
                 acc]
                [(and (pair? req-e) (eq? (car req-e) 'prefix-in))
                 (cons (list (syntax-e (cadddr req-e)) (cadddr req-e)) acc)]
                [(and (pair? req-e) (eq? (car req-e) 'rename-in))
                 (for/fold ([bs '()]) ([mapping (in-list (cddr req-e))])
                   (cons (list (syntax-e (car mapping)) (car mapping)) bs))]
                [else acc]))]
           [(pair? e)
            (apply append (map get-require-bindings (syntax-e stx)))]
           [else '()])]))
    (define require-bindings (get-require-bindings stx))
    (when (null? require-bindings)
      '())
    (define binding-names (map first require-bindings))
    (define (collect-references stx)
      (cond
        [(not (syntax? stx)) '()]
        [else
         (define e (syntax-e stx))
         (cond
           [(identifier? stx)
            (if (memq (syntax-e stx) binding-names) '() (list stx))]
           [(pair? e)
            (append (collect-references (car e))
                    (apply append (map collect-references (cdr e))))]
           [else '()])]))
    (define all-references (collect-references stx))
    (define referenced-names (map syntax-e all-references))
    (define used-names (list->set referenced-names))
    (for/list ([binding (in-list require-bindings)]
               #:when (not (set-member? used-names (first binding))))
      (define id (second binding))
      (diagnostic path (or (syntax-line id) 1) (or (syntax-column id) 1)
                  'info 'reachability/unused-require
                  (format "Required binding ~a is not used" (first binding))))))
