#lang racket/base

(require
  drracket/check-syntax
  racket/contract
  racket/port
  racket/path
  racket/string
  racket/list
  racket/set
  "diagnostic.rkt")

(provide
  (contract-out
    [check-syntax-analyze (-> path-string? (listof diagnostic?))]))

;; Analyze a file using check-syntax's show-content
(define (check-syntax-analyze path)
  (define results (show-content path))
  
  ;; Parse the results to extract diagnostics
  (define diagnostics '())
  
  (for ([result (in-list results)])
    (define method (vector-ref result 0))
    (cond
      ;; Unused binder (variable defined but never used)
      [(eq? method 'syncheck:unused-binder)
       (define start (vector-ref result 1))
       (define end (vector-ref result 2))
       (set! diagnostics
             (cons (diagnostic path 1 1 'info 'check-syntax/unused-variable
                               (format "Variable at position ~a-~a is defined but never used" start end))
                   diagnostics))]
      ;; Unused require
      [(eq? method 'syncheck:add-unused-require)
       (define start (vector-ref result 1))
       (define end (vector-ref result 2))
       (set! diagnostics
             (cons (diagnostic path 1 1 'info 'check-syntax/unused-require
                               (format "Unused require at position ~a-~a" start end))
                   diagnostics))]))
  
  diagnostics)
