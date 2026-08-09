#lang racket/base

;; Check-Syntax integration module for racket-linter
;;
;; This module provides integration with DrRacket's check-syntax API
;; for precise detection of unused variables and unused requires.
;;
;; Unlike the regex-based rules, check-syntax uses macro expansion
;; and binding analysis to provide accurate results.

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

;; Analyze a file using DrRacket's check-syntax API.
;;
;; This function uses show-content to get detailed information about
;; the file's syntax, including:
;; - Unused variables (syncheck:unused-binder)
;; - Unused requires (syncheck:add-unused-require)
;;
;; Returns a list of diagnostics for any issues found.
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
