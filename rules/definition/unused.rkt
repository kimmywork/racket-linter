#lang racket/base

(require
  racket/list
  racket/string
  racket/port
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide definition/unused)

(define-rule definition/unused
  #:id 'definition/unused
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (port->string (open-input-file path)))
    (define lines (regexp-split #px"\n" text))
    (for/fold ([acc '()]) ([line lines] [ln (in-naturals 1)])
      (define trimmed (string-trim line))
      (cond
        [(regexp-match? #px"^\\(define[\\s]+(?:\\(?)([^\\s()]+)" trimmed)
         (define m (regexp-match #px"^\\(define[\\s]+(?:\\(?)([^\\s()]+)" trimmed))
         (if m
             (append acc (list (diagnostic path ln 1 'info 'definition/unused
                                                         (format "Definition ~a appears unused" (second m)))))
             acc)]
        [else acc]))))
