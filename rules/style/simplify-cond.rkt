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

(provide style/simplify-cond)

;; Detect cond expressions that could be simplified
;; This rule looks for:
;; - cond with only one clause (could be if)
;; - cond with else as the only clause
;; - cond with #t as test (could be else)

(define-rule style/simplify-cond
  #:id 'style/simplify-cond
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (port->string (open-input-file path)))
    (define lines (string-split text "\n"))
    
    (define diagnostics '())
    
    (for ([line (in-list lines)] [ln (in-naturals 1)])
      (define trimmed (string-trim line))
      
      ;; Check for cond with #t as test (on same line or next line)
      (when (or (regexp-match? #px"\\(cond\\s+\\[\\s*#t" trimmed)
                (regexp-match? #px"^\\[\\s*#t" trimmed))
        (set! diagnostics
              (cons (diagnostic path ln 1 'info 'style/simplify-cond
                                "Use 'else' instead of '#t' in cond")
                    diagnostics)))
      
      ;; Check for cond with only one clause (could be if)
      (when (regexp-match? #px"\\(cond\\s+\\[[^\\]]+\\]\\s*\\)" trimmed)
        (set! diagnostics
              (cons (diagnostic path ln 1 'info 'style/simplify-cond
                                "cond with single clause could be simplified to if")
                    diagnostics))))
    
    diagnostics))
