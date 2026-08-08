#lang racket/base

(require
  racket/port
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(define-syntax-rule (define-style-rule name id severity check-expr)
  (define-rule name
    #:id id
    #:severity severity
    #:config-keys (hash)
    (lambda (stx path)
      (check-expr stx path))))

(provide style/line-length)

(define-style-rule style/line-length 'style/line-length 'warning
  (lambda (stx path)
    (define lines (regexp-split #px"\n" (port->string (open-input-file path))))
    (define max-length 102)
    (for/fold ([acc '()]) ([line lines] [ln (in-naturals 1)])
      (if (> (string-length line) max-length)
          (append acc (list (diagnostic path ln 1 'warning 'style/line-length
                                              (format "Line length ~a exceeds ~a" (string-length line) max-length))))
          acc))))
