#lang racket/base

(require
  racket/port
  racket/contract/base
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide style/sexpr-depth)

(define-rule style/sexpr-depth
  #:id 'style/sexpr-depth
  #:severity 'warning
  #:config-keys (hash)
  (lambda (stx path)
    (if (not (syntax? stx))
        '()
        (let ([max-depth 10])
          (let recurse ([stx stx] [depth 0])
            (define is-list? (and (pair? (syntax-e stx)) (list? (syntax-e stx))))
            (define results
              (if (and is-list? (>= depth max-depth))
                  (list (diagnostic path (or (syntax-line stx) 1) (or (syntax-column stx) 1) 'warning 'style/sexpr-depth
                                              (format "S-expression depth ~a exceeds ~a" depth max-depth)))
                  '()))
            (if is-list?
                (append results
                        (apply append
                               (for/list ([sub (in-list (syntax-e stx))])
                                 (recurse sub (+ depth 1)))))
                results))))))
