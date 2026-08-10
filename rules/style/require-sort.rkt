#lang racket/base

(require
  racket/list
  racket/string
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt")

(provide style/require-sort)

(define (parts-of stx)
  (define parts (and (syntax? stx) (syntax->list stx)))
  (and (pair? parts) parts))
(define (head-name parts)
  (and (pair? parts) (identifier? (first parts)) (syntax-e (first parts))))

(define (spec-rank spec)
  (define parts (parts-of spec))
  (cond
    [(and parts (eq? (head-name parts) 'for-label)) 0]
    [(and parts (eq? (head-name parts) 'for-syntax)) 1]
    [(string? (syntax-e spec)) 3]
    [else 2]))

(define (spec-key spec)
  (format "~s" (syntax->datum spec)))

(define (spec<? left right)
  (or (< (spec-rank left) (spec-rank right))
      (and (= (spec-rank left) (spec-rank right))
           (string<? (spec-key left) (spec-key right)))))

(define (walk stx path)
  (define parts (parts-of stx))
  (cond
    [(not parts) '()]
    [(memq (head-name parts) '(quote quasiquote syntax quasisyntax)) '()]
    [else
     (define local
       (if (eq? (head-name parts) 'require)
           (let ([specs (rest parts)])
             (if (equal? specs (sort specs spec<?))
                 '()
                 (list
                  (diagnostic path
                              (or (syntax-line stx) 1)
                              (or (syntax-column stx) 0)
                              'info 'style/require-sort
                              "require arguments should be ordered by phase, module kind, and name"))))
           '()))
     (append local
             (append-map (lambda (child) (walk child path)) (rest parts)))]))

(define-rule style/require-sort
  #:id 'style/require-sort
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (walk stx path)))
