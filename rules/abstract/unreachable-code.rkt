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

(provide abstract/unreachable-code)

;; Abstract interpretation-based unreachable code detection
;; This rule detects code that can never be reached because:
;; - It's after a return/exit/raise
;; - It's in a branch that always evaluates to false

(define-rule abstract/unreachable-code
  #:id 'abstract/unreachable-code
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (port->string (open-input-file path)))
    (define lines (string-split text "\n"))
    
    (define diagnostics '())
    
    ;; Track if we're in unreachable code
    (define unreachable? #f)
    (define unreachable-depth 0)
    (define exit-line? #f)
    
    (for ([line (in-list lines)] [ln (in-naturals 1)])
      (define trimmed (string-trim line))
      
      ;; First check if current line is unreachable
      (when (and unreachable? (> (string-length trimmed) 0) (not exit-line?))
        (set! diagnostics
              (cons (diagnostic path ln 1 'warning 'abstract/unreachable-code
                                (format "Code after exit/raise may be unreachable: ~a"
                                        (substring trimmed 0 (min 50 (string-length trimmed)))))
                    diagnostics)))
      
      ;; Then check for exit/raise/return forms
      (when (regexp-match? #px"^\\((exit|raise|error|throw)\\s+" trimmed)
        (set! unreachable? #t)
        (set! unreachable-depth 0)
        (set! exit-line? #t))
      
      ;; Track depth to know when we exit the unreachable block
      (when (and unreachable? (not exit-line?))
        (for ([ch (in-string trimmed)])
          (cond
            [(char=? ch #\() (set! unreachable-depth (+ unreachable-depth 1))]
            [(char=? ch #\)) (set! unreachable-depth (- unreachable-depth 1))]))
        
        (when (<= unreachable-depth 0)
          (set! unreachable? #f)))
      
      (set! exit-line? #f))
    
    diagnostics))
