#lang racket/base

(require
  racket/list
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt")

(provide style/simplify-cond)

(define (parts-of stx)
  (define parts (and (syntax? stx) (syntax->list stx)))
  (and (pair? parts) parts))
(define (head-name parts)
  (and (pair? parts) (identifier? (first parts)) (syntax-e (first parts))))

(define (clause-test clause)
  (define parts (parts-of clause))
  (and parts (pair? parts) (first parts)))

(define (walk stx path)
  (define parts (parts-of stx))
  (cond
    [(not parts) '()]
    [(memq (head-name parts) '(quote quasiquote syntax quasisyntax)) '()]
    [else
     (define local
       (if (eq? (head-name parts) 'cond)
           (let ([clauses (rest parts)])
             (append
              (if (= (length clauses) 1)
                  (list
                   (diagnostic path (or (syntax-line stx) 1)
                               (or (syntax-column stx) 0)
                               'info 'style/simplify-cond
                               "single-clause cond can usually be an if or when"))
                  '())
              (if (and (pair? clauses)
                       (let ([test (clause-test (last clauses))])
                         (and test (eq? (syntax-e test) #t))))
                  (let ([test (clause-test (last clauses))])
                    (list
                     (diagnostic path (or (syntax-line test) 1)
                                 (or (syntax-column test) 0)
                                 'info 'style/simplify-cond
                                 "use else instead of #t for the final cond clause")))
                  '())))
           '()))
     (append local
             (append-map (lambda (child) (walk child path)) (rest parts)))]))

(define-rule style/simplify-cond
  #:id 'style/simplify-cond
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (walk stx path)))
