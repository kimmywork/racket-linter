#lang racket/base

(require
  racket/port
  racket/string
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt")

(provide review/module-declaration)

(define-rule review/module-declaration
  #:id 'review/module-declaration
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (call-with-input-file path port->string))
    (if (regexp-match? #px"(?m:^#lang\\s+\\S+)" text)
        '()
        (list
         (diagnostic path 1 0 'warning 'review/module-declaration
                     "missing module (#lang) declaration")))))
