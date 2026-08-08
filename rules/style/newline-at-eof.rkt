#lang racket/base

(require
  racket/port
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide style/newline-at-eof)

(define-rule style/newline-at-eof
  #:id 'style/newline-at-eof
  #:severity 'warning
  #:config-keys (hash)
  (lambda (stx path)
    (define content (call-with-input-file path port->string))
    (define len (string-length content))
    (if (and (> len 0) (equal? (string-ref content (- len 1)) #\newline))
        '()
        (list (diagnostic path 1 1 'warning 'style/newline-at-eof "File does not end with a newline")))))
