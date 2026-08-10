#lang racket/base

(require
  racket/list
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt")

(provide style/provide-sort)

(define (parts-of stx)
  (define parts (and (syntax? stx) (syntax->list stx)))
  (and (pair? parts) parts))
(define (head-name parts)
  (and (pair? parts) (identifier? (first parts)) (syntax-e (first parts))))
(define (spec-key spec) (format "~s" (syntax->datum spec)))

(define (walk stx path)
  (define parts (parts-of stx))
  (cond
    [(not parts) '()]
    [(memq (head-name parts) '(quote quasiquote syntax quasisyntax)) '()]
    [else
     (define local
       (if (eq? (head-name parts) 'provide)
           (let* ([specs (rest parts)]
                  [ordered (sort specs string<? #:key spec-key)])
             (if (equal? specs ordered)
                 '()
                 (list
                  (diagnostic path
                              (or (syntax-line stx) 1)
                              (or (syntax-column stx) 0)
                              'info 'style/provide-sort
                              "provide arguments should be sorted by exported form"))))
           '()))
     (append local
             (append-map (lambda (child) (walk child path)) (rest parts)))]))

(define-rule style/provide-sort
  #:id 'style/provide-sort
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (walk stx path)))
