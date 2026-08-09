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

(provide style/provide-sort)

;; Detect unsorted provide forms
;; A provide form is considered unsorted if its arguments are not in alphabetical order

(define-rule style/provide-sort
  #:id 'style/provide-sort
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (port->string (open-input-file path)))
    (define lines (string-split text "\n"))
    
    (for/fold ([acc '()]) ([line (in-list lines)] [ln (in-naturals 1)])
      (define trimmed (string-trim line))
      ;; Check if this line starts a provide form
      (if (regexp-match? #px"^\\(provide\\s+" trimmed)
          ;; Extract the provide arguments
          (let* ([args-str (regexp-replace #px"^\\(provide\\s+" trimmed "")]
                 [args-str (regexp-replace #px"\\)\\s*$" args-str "")]
                 [args (string-split args-str)]
                 [sorted-args (sort args string<?)])
            (if (equal? args sorted-args)
                acc
                (cons (diagnostic path ln 1 'info 'style/provide-sort
                                  "Provide arguments are not sorted alphabetically")
                      acc)))
          acc))))
