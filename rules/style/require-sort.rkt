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

(provide style/require-sort)

;; Detect unsorted require forms
;; A require form is considered unsorted if its arguments are not in alphabetical order

(define-rule style/require-sort
  #:id 'style/require-sort
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (port->string (open-input-file path)))
    (define lines (string-split text "\n"))
    
    (for/fold ([acc '()]) ([line (in-list lines)] [ln (in-naturals 1)])
      (define trimmed (string-trim line))
      ;; Check if this line starts a require form
      (if (regexp-match? #px"^\\(require\\s+" trimmed)
          ;; Extract the require arguments
          (let* ([args-str (regexp-replace #px"^\\(require\\s+" trimmed "")]
                 [args-str (regexp-replace #px"\\)\\s*$" args-str "")]
                 [args (string-split args-str)]
                 [sorted-args (sort args string<?)])
            (if (equal? args sorted-args)
                acc
                (cons (diagnostic path ln 1 'info 'style/require-sort
                                  "Require arguments are not sorted alphabetically")
                      acc)))
          acc))))
