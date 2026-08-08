#lang racket/base

(require
  racket/port
  racket/contract/base
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide style/file-length)

(define-rule style/file-length
  #:id 'style/file-length
  #:severity 'warning
  #:config-keys (hash)
  #:layer 'text
  (lambda (stx path)
    (define max-lines 1000)
    (define text (call-with-input-file path port->string))
    (define lines (regexp-split #px"\n" text))
    (define len (length lines))
    (if (> len max-lines)
        (list (diagnostic path 1 1 'warning 'style/file-length
                            (format "File length ~a exceeds ~a lines" len max-lines)))
        '())))
