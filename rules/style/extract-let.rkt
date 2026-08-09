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

(provide style/extract-let)

;; Detect repeated expressions that could be extracted to a let binding
;; This rule looks for expressions that appear multiple times in the same function

(define-rule style/extract-let
  #:id 'style/extract-let
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (port->string (open-input-file path)))
    (define lines (string-split text "\n"))
    
    ;; Find all expressions in parentheses
    (define expressions (make-hash))
    (for ([line (in-list lines)] [ln (in-naturals 1)])
      (define trimmed (string-trim line))
      ;; Find all parenthesized expressions
      (define exprs (regexp-match* #px"\\([^()]+\\)" trimmed))
      (for ([expr (in-list exprs)])
        ;; Normalize the expression
        (define normalized (string-trim expr))
        (when (> (string-length normalized) 5) ; Skip very short expressions
          (hash-update! expressions normalized
                        (lambda (old) (cons ln old))
                        '()))))
    
    ;; Find expressions that appear more than once
    (define diagnostics '())
    (for ([(expr locations) (in-hash expressions)])
      (when (>= (length locations) 2)
        (set! diagnostics
              (cons (diagnostic path (first locations) 1 'info 'style/extract-let
                                (format "Expression '~a' appears ~a times; consider extracting to a let binding"
                                        expr (length locations)))
                    diagnostics))))
    
    diagnostics))
