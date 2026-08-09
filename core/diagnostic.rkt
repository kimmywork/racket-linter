#lang racket/base

;; Diagnostic module for racket-linter
;; 
;; A diagnostic represents a single finding from a linter rule.
;; Each diagnostic has a location (path, line, col), severity level,
;; rule identifier, and a human-readable message.

(require racket/contract/base)

(provide
 (contract-out
  [struct diagnostic ([path path-string?]
                      [line exact-positive-integer?]
                      [col exact-nonnegative-integer?]
                      [severity (one-of/c 'error 'warning 'info)]
                      [rule-id symbol?]
                      [message string?])]))

;; Represents a single diagnostic finding from a linter rule.
;; 
;; Fields:
;;   path - file path where the diagnostic was found
;;   line - line number (1-based)
;;   col - column number (0-based)
;;   severity - 'error, 'warning, or 'info
;;   rule-id - symbol identifying the rule that produced this diagnostic
;;   message - human-readable description of the issue
(struct diagnostic (path line col severity rule-id message)
  #:transparent)
