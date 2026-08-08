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

(provide reachability/undefined)

(define-rule reachability/undefined
  #:id 'reachability/undefined
  #:severity 'warning
  #:config-keys (hash)
  #:layer 'syntax
  (lambda (stx path config)
    (define built-ins
      (set 'define 'define-values 'define-syntax 'define-syntaxes
           'lambda 'let 'let* 'letrec 'if 'cond 'case 'match
           'and 'or 'not 'begin 'set! 'quote 'quasiquote 'unquote 'unquote-splicing
           'display 'displayln 'printf 'newline 'list 'cons 'car 'cdr 'cadr 'cddr
           'null? 'pair? 'list? 'eq? 'equal? 'number? 'string? 'symbol? 'boolean?
           '+ '- '* '/ '= '< '> '<= '>= 'remainder 'modulo 'add1 'sub1 'expt 'abs
           'apply 'map 'filter 'foldl 'foldr 'for/list 'for/fold 'in-range 'in-list
           'for/sum 'for/product 'for/and 'for/or 'for/first 'for/last
           'error 'raise 'with-handlers 'exn? 'exn-message 'exn-continuation-marks
           'require 'provide 'module 'racket/base 'racket
           'only-in 'except-in 'prefix-in 'rename-in 'file 'for-syntax 'for-template
           'for-label 'for-meta 'for-space 'all-defined-out 'all-from-out
           'struct 'struct-out 'define-struct 'type-id 'constructor-id
           'mutator-id 'accessor-id 'predicate-id
           'module+ 'submod 'begin-for-syntax 'begin-for-template
           '#%app '#%datum '#%top '#%top-interaction '#%module-begin))
    (define (is-built-in? name)
      (set-member? built-ins name))
    (define (is-definition? stx)
      (and (syntax? stx)
           (pair? (syntax-e stx))
           (let ([head (syntax-e (car (syntax-e stx)))])
             (and (symbol? head)
                  (memq head '(define define-values define-syntax define-syntaxes))))))
    (define (get-def-name stx)
      (and (is-definition? stx)
           (pair? (cdr (syntax-e stx)))
           (syntax-e (cadr (syntax-e stx)))))
    (define (collect-info stx)
      (cond
        [(not (syntax? stx)) '()]
        [else
         (define e (syntax-e stx))
         (cond
           [(is-definition? stx)
            (define def-name (get-def-name stx))
            (cons (list 'defined def-name stx)
                  (collect-info (caddr (syntax-e stx))))]
           [(and (pair? e) (not (null? (syntax-e stx))))
            (define head (syntax-e (car (syntax-e stx))))
            (if (and (symbol? head) (is-built-in? head))
                (apply append (map collect-info (cdr (syntax-e stx))))
                (cons (list 'ref head (car (syntax-e stx)))
                      (apply append (map collect-info (cdr (syntax-e stx))))))]
           [(pair? e)
            (apply append (map collect-info (syntax-e stx)))]
           [else '()])]))
    (define info (collect-info stx))
    (define defined-names (map second (filter (lambda (x) (eq? (first x) 'defined)) info)))
    (define references (filter (lambda (x) (eq? (first x) 'ref)) info))
    (define defined-set (list->set defined-names))
    (for/list ([ref (in-list references)]
               #:when (not (set-member? defined-set (second ref))))
      (define id (third ref))
      (diagnostic path (or (syntax-line id) 1) (or (syntax-column id) 1)
                  'warning 'reachability/undefined
                  (format "Reference to undefined identifier ~a" (second ref))))))
