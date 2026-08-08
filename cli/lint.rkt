#lang racket/base
(require racket/string racket/path racket/list racket/contract/base racket/function racket/dict
         "../core/diagnostic.rkt" "../core/rule.rkt" "../core/engine.rkt"
         "../rules/style/line-length.rkt" "../rules/style/trailing-whitespace.rkt"
         "../rules/style/newline-at-eof.rkt" "../rules/style/sexpr-depth.rkt"
         "../rules/style/definition-length.rkt" "../rules/style/file-length.rkt"
         "../rules/definition/unused.rkt" "../rules/reachability/undefined.rkt"
         "../rules/reachability/unused-require.rkt" "../rules/export/unused.rkt"
         "../rules/module/require-provide.rkt"
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
  (define all-rules
    (list style/line-length style/trailing-whitespace style/newline-at-eof
          style/sexpr-depth style/definition-length style/file-length
          definition/unused reachability/undefined reachability/unused-require
          export/unused module/require-provide))
  (define default-config (hash 'definition/unused (hash 'enabled #f)
                               'reachability/unused-require (hash 'enabled #f)
                               'export/unused (hash 'enabled #f)
                               'module/require-provide (hash 'enabled #f)))
  (define user-config-file (build-path dir ".racket-linter.rkt"))
  (define user-config
    (if (file-exists? user-config-file)
        (with-handlers ([exn? (lambda (e) default-config)])
          (parameterize ([current-namespace (make-base-namespace)])
            (eval (call-with-input-file user-config-file read))))
        default-config))
  (define (merge-configs a b)
    (for/fold ([result a]) ([(k v) (in-hash b)])
      (hash-set result k (if (hash-has-key? result k)
                             (for/fold ([inner (hash-ref result k)]) ([(ik iv) (in-hash v)])
                               (hash-set inner ik iv))
                             v))))
  (define merged-config (merge-configs default-config user-config))
  (define diagnostics
    (apply append (map (lambda (f) (run-file all-rules merged-config f)) files)))
  (print-diagnostics diagnostics)
  (exit (if (null? diagnostics) 0 1)))
