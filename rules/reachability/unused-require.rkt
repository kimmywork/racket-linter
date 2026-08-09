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
            (for/fold ([acc '()]) ([req (in-list (cdr e))])
              (define req-e (syntax-e req))
              (define req-head
                (and (pair? req-e) (syntax-e (car req-e))))
              (cond
                [(symbol? req-e)
                 (cons (list req-e req) acc)]
                [(eq? req-head 'only-in)
                 (for/fold ([bindings acc])
                           ([name (in-list (cddr req-e))])
                   (cons (list (syntax-e name) name) bindings))]
                [(eq? req-head 'except-in) acc]
                [(eq? req-head 'prefix-in)
                 (if (>= (length req-e) 3)
                     (cons (list (syntax-e (second req-e))
                                 (second req-e))
                           acc)
                     acc)]
                [(eq? req-head 'rename-in)
                 (for/fold ([bindings acc])
                           ([mapping (in-list (cddr req-e))]
                            #:when (and (pair? (syntax-e mapping))
                                        (pair? (cdr (syntax-e mapping)))))
                   (define mapping-e (syntax-e mapping))
                   (cons (list (syntax-e (second mapping-e))
                               (second mapping-e))
                         bindings))]
                [else acc]))]
           [(pair? e)
            (apply append (map get-require-bindings e))]
           [else '()])]))
    (define require-bindings (get-require-bindings stx))
    (when (null? require-bindings)
      '())
    (define (collect-references stx)
      (cond
        [(not (syntax? stx)) '()]
        [(identifier? stx) (list stx)]
        [else
         (define e (syntax-e stx))
         (if (pair? e)
             (let ([head (syntax-e (car e))])
               (cond
                 [(memq head '(provide require quote quasiquote syntax quasisyntax)) '()]
                 [(eq? head 'define)
                  (apply append (map collect-references (cddr e)))]
                 [(eq? head 'define-values)
                  (apply append (map collect-references (cddr e)))]
                 [(memq head '(lambda let let* letrec))
                  (apply append (map collect-references (cddr e)))]
                 [else
                  (apply append (map collect-references e))]))
             '())]))
    (define all-references (collect-references stx))
    (define referenced-names (map syntax-e all-references))
    (define used-names (list->set referenced-names))
    (for/list ([binding (in-list require-bindings)]
               #:when (not (set-member? used-names (first binding))))
      (define id (second binding))
      (diagnostic path (or (syntax-line id) 1) (or (syntax-column id) 1)
                  'info 'reachability/unused-require
                  (format "Required binding ~a is not used" (first binding))))))
