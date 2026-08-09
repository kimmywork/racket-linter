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
    (define (provided-specs value)
      (define e (syntax-e value))
      (cond
        [(symbol? e) (list (list e value))]
        [(and (pair? e)
              (eq? (syntax-e (car e)) 'only-in))
         (for/list ([name (in-list (cddr e))])
           (list (syntax-e name) name))]
        [(and (pair? e)
              (eq? (syntax-e (car e)) 'rename-out))
         (for/list ([mapping (in-list (cdr e))]
                    #:when (pair? (syntax-e mapping)))
           (define mapping-e (syntax-e mapping))
           (list (syntax-e (second mapping-e))
                 (second mapping-e)))]
        [else '()]))
    (define (get-provided-ids stx)
      (cond
        [(not (syntax? stx)) '()]
        [else
         (define e (syntax-e stx))
         (cond
           [(and (pair? e) (eq? (syntax-e (car e)) 'provide))
            (apply append (map provided-specs (cdr e)))]
           [(pair? e)
            (apply append (map get-provided-ids e))]
           [else '()])]))
    (define provided (get-provided-ids stx))
    (when (null? provided)
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
    (for/list ([prov (in-list provided)]
               #:when (not (set-member? used-names (first prov))))
      (define id (second prov))
      (diagnostic path (or (syntax-line id) 1) (or (syntax-column id) 1)
                  'info 'export/unused
                  (format "Exported identifier ~a is not used within the module" (first prov))))))
