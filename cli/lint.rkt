#lang racket/base
(require racket/string racket/path racket/list racket/contract/base racket/function racket/dict racket/port racket/system
         "../core/diagnostic.rkt" "../core/rule.rkt" "../core/engine.rkt"
         "../rules/style/line-length.rkt" "../rules/style/trailing-whitespace.rkt"
         "../rules/style/newline-at-eof.rkt" "../rules/style/sexpr-depth.rkt"
         "../rules/style/definition-length.rkt" "../rules/style/file-length.rkt"
         "../rules/definition/unused.rkt" "../rules/reachability/undefined.rkt"
         "../rules/reachability/unused-require.rkt" "../rules/reachability/unused-require-expand.rkt"
         "../rules/export/unused.rkt"
         "../rules/module/require-provide.rkt"
         (for-syntax racket/base))
(provide run)
(module+ main (run (vector->list (current-command-line-arguments))))
(define (usage)
  (displayln "Usage: raco lint [--fix] [--format] [--no-config] <directory>")
  (displayln "")
  (displayln "Options:")
  (displayln "  --fix        Auto-fix trailing whitespace and missing EOF newline")
  (displayln "  --format     Format all files using raco fmt (requires fmt package)")
  (displayln "  --no-config  Ignore .racket-linter.rkt config file")
  (exit 1))
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

;; Auto-fix support
(define (fix-trailing-whitespace lines)
  (map string-trim lines))

(define (fix-newline-at-eof lines)
  (if (and (not (null? lines)) (string=? (last lines) ""))
      lines
      (append lines (list ""))))

(define (apply-fixes path diagnostics)
  (define text (call-with-input-file path port->string))
  (define lines (string-split text "\n" #:trim? #f))
  (define has-trailing? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/trailing-whitespace)) diagnostics))
  (define has-no-eof-newline? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/newline-at-eof)) diagnostics))
  (define fixed-lines
    (let ([l lines])
      (when has-trailing?
        (set! l (fix-trailing-whitespace l)))
      (when has-no-eof-newline?
        (set! l (fix-newline-at-eof l)))
      l))
  (define fixed-text (string-join fixed-lines "\n"))
  (unless (string=? text fixed-text)
    (displayln (format "Fixed: ~a" path))
    (call-with-output-file path
      (lambda (out) (display fixed-text out))
      #:exists 'replace)))

;; Format support using raco fmt
(define (format-file path-str)
  (define result
    (with-handlers ([exn? (lambda (e) (eprintf "Format error: ~a: ~a\n" path-str (exn-message e)) #f)])
      (define text (call-with-input-file path-str port->string))
      (define dir (path-only (string->path path-str)))
      (define formatted
        (parameterize ([current-directory dir])
          (with-output-to-string
            (lambda ()
              (system (format "raco fmt ~a" path-str))))))
      (if (and (string? formatted) (not (string=? text formatted)))
          (begin
            (displayln (format "Formatted: ~a" path-str))
            (call-with-output-file path-str
              (lambda (out) (display formatted out))
              #:exists 'replace)
            #t)
          #f)))
  result)

(define (parse-args args)
  (define fix? #f)
  (define format? #f)
  (define no-config? #f)
  (define dir #f)
  (for ([arg (in-list args)])
    (cond
      [(string=? arg "--fix") (set! fix? #t)]
      [(string=? arg "--format") (set! format? #t)]
      [(string=? arg "--no-config") (set! no-config? #t)]
      [(not (string-prefix? arg "--")) (set! dir arg)]
      [else (eprintf "Unknown option: ~a\n" arg) (usage)]))
  (values fix? format? no-config? dir))

(define (run args)
  (define-values (fix? format? no-config? dir) (parse-args args))
  (unless dir (usage))
  (unless (directory-exists? dir)
    (eprintf "Error: ~a is not a directory\n" dir)
    (exit 1))
  (define files (find-rkt-files dir))
  (define all-rules
    (list style/line-length style/trailing-whitespace style/newline-at-eof
          style/sexpr-depth style/definition-length style/file-length
          definition/unused reachability/undefined reachability/unused-require
          reachability/unused-require-expand
          export/unused module/require-provide))
  (define user-config-file (build-path dir ".racket-linter.rkt"))
  (define user-config
    (if (and (not no-config?) (file-exists? user-config-file))
        (with-handlers ([exn? (lambda (e) (hash))])
          (parameterize ([current-namespace (make-base-namespace)])
            (eval (call-with-input-file user-config-file read))))
        (hash)))
  (define merged-config user-config)
  ;; Format files if requested
  (when format?
    (displayln "Formatting files...")
    (for ([f (in-list files)])
      (format-file f)))
  (define diagnostics
    (apply append (map (lambda (f) (run-file all-rules merged-config f)) files)))
  (when fix?
    ;; Group diagnostics by file
    (define by-file (make-hash))
    (for ([d (in-list diagnostics)])
      (hash-update! by-file (diagnostic-path d) (lambda (old) (cons d old)) '()))
    (for ([(path diags) (in-hash by-file)])
      (apply-fixes path diags)))
  (print-diagnostics diagnostics)
  (exit (if (null? diagnostics) 0 1)))
