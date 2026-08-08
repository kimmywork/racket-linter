#lang racket/base
(require racket/string racket/path racket/list racket/contract/base racket/function
        "../core/diagnostic.rkt" "../core/rule.rkt" "../core/engine.rkt"
        "../rules/style/line-length.rkt" "../rules/style/trailing-whitespace.rkt"
        "../rules/style/newline-at-eof.rkt" "../rules/style/sexpr-depth.rkt"
        "../rules/style/definition-length.rkt" "../rules/definition/unused.rkt"
        (for-syntax racket/base))
(provide run)
(module+ main (run (vector->list (current-command-line-arguments))))
(define (usage) (displayln "Usage: raco lint <directory>") (exit 1))
(define (find-rkt-files dir)
  (for/list ([f (in-directory dir)]
             #:when (and (file-exists? f)
                         (regexp-match? #px"\\.rkt$" (path->string f))))
    (path->string f)))
(define (print-diagnostics diagnostics)
  (for-each
    (lambda (d)
      (printf "~a:~a:~a: [~a] ~a: ~a\n"
              (diagnostic-path d) (diagnostic-line d) (diagnostic-col d)
              (diagnostic-severity d) (diagnostic-rule-id d) (diagnostic-message d)))
    diagnostics))
(define (run args)
  (when (null? args) (usage))
  (define dir (car args))
  (unless (directory-exists? dir)
    (eprintf "Error: ~a is not a directory\n" dir)
    (exit 1))
  (define files (find-rkt-files dir))
  (define rules
    (list style/line-length style/trailing-whitespace style/newline-at-eof
          style/sexpr-depth style/definition-length definition/unused))
  (define diagnostics
    (apply append (map (lambda (f) (run-file rules (hash) f)) files)))
  (print-diagnostics diagnostics)
  (exit (if (null? diagnostics) 0 1)))
