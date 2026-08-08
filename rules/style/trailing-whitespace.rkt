#lang racket/base

(require
  racket/port
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide style/trailing-whitespace)

(define-rule style/trailing-whitespace
  #:id 'style/trailing-whitespace
  #:severity 'warning
  #:config-keys (hash)
  (lambda (stx path)
    (define lines (regexp-split #px"\n" (port->string (open-input-file path))))
    (for/fold ([acc '()]) ([line lines] [ln (in-naturals 1)])
      (if (regexp-match? #px"\\s+$" line)
          (append acc (list (diagnostic path ln 1 'warning 'style/trailing-whitespace "Line has trailing whitespace")))
          acc))))
