#lang racket/base

(require
  racket/contract/base
  racket/function
  racket/path
  racket/list
  racket/string
  racket/match
  "core/diagnostic.rkt"
  "core/rule.rkt"
  "core/engine.rkt"
  "rules/style/line-length.rkt"
  "rules/style/trailing-whitespace.rkt"
  "rules/style/newline-at-eof.rkt"
  "rules/definition/unused.rkt"
  (for-syntax racket/base))

(provide
  (contract-out
    [run ((listof path-string?) . -> . (listof diagnostic?))]))

(define (load-config)
  (hash))

(define (default-rules)
  (list
    style/line-length
    style/trailing-whitespace
    style/newline-at-eof
    definition/unused))

(define (run paths)
  (define config (load-config))
  (define rules (default-rules))
  (define all-diagnostics '())
  
  (for-each
    (lambda (path)
      (define diagnostics (run-file rules config path))
      (set! all-diagnostics (append all-diagnostics diagnostics)))
    paths)
  
  (sort all-diagnostics
        (lambda (a b)
          (or (< (diagnostic-line a) (diagnostic-line b))
              (and (= (diagnostic-line a) (diagnostic-line b))
                   (< (diagnostic-col a) (diagnostic-col b)))))))
