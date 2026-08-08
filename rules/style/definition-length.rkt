#lang racket/base

(require
  racket/string
  racket/port
  racket/contract/base
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide style/definition-length)

(define-rule style/definition-length
  #:id 'style/definition-length
  #:severity 'warning
  #:config-keys (hash)
  #:layer 'text
  (lambda (stx path)
    (define max-length 66)
    (define text (call-with-input-file path port->string))
    (define lines (regexp-split #px"\n" text))
    (define results '())
    (let loop ([lines lines] [ln 1] [state 'out] [def-start 0])
      (cond
        [(null? lines) results]
        [(eq? state 'out)
         (define trimmed (string-trim (car lines)))
         (if (regexp-match? #px"^\\(define" trimmed)
             (loop (cdr lines) (+ ln 1) 'in ln)
             (loop (cdr lines) (+ ln 1) 'out 0))]
        [(eq? state 'in)
         (define trimmed (string-trim (car lines)))
         (define len (- ln def-start))
         (if (or (and (> len max-length)
                      (set! results (append results (list (diagnostic path def-start 1 'warning 'style/definition-length
                                                          (format "Definition length ~a exceeds ~a lines" len max-length))))))
                 (and (> (string-length trimmed) 0)
                      (char=? (string-ref trimmed 0) #\()
                      (not (regexp-match? #px"^#;" trimmed))))
             (loop (cdr lines) (+ ln 1) 'out 0)
             (loop (cdr lines) (+ ln 1) 'in def-start))]))))
