#lang racket/base
(require json
         raco/command-name
         racket/string racket/path racket/list racket/contract/base racket/function racket/dict racket/port racket/system
         "../core/diagnostic.rkt" "../core/rule.rkt" "../core/engine.rkt" "../core/project.rkt"
         "../rules/style/line-length.rkt" "../rules/style/trailing-whitespace.rkt"
         "../rules/style/newline-at-eof.rkt" "../rules/style/sexpr-depth.rkt"
         "../rules/style/definition-length.rkt" "../rules/style/file-length.rkt"
         "../rules/style/naming-convention.rkt" "../rules/style/require-sort.rkt"
         "../rules/style/provide-sort.rkt" "../rules/style/extract-let.rkt"
         "../rules/style/simplify-cond.rkt"
         "../rules/definition/unused.rkt" "../rules/reachability/undefined.rkt"
         "../rules/reachability/unused-require.rkt" "../rules/reachability/unused-require-expand.rkt"
         "../rules/export/unused.rkt"
         "../rules/module/require-provide.rkt"
         "../rules/abstract/type-error.rkt" "../rules/abstract/unreachable-code.rkt"
         "../rules/check-syntax/unused.rkt"
         (for-syntax racket/base))
(provide run)
(define (usage [status 1])
  (displayln "Usage: raco lint [options] <directory>")
  (displayln "")
  (displayln "Options:")
  (displayln "  --fix              Auto-fix trailing whitespace and missing EOF newline")
  (displayln "  --format           Format all files using raco fmt (requires fmt package)")
  (displayln "  --no-config        Ignore .racket-linter.rkt config file")
  (displayln "  --config <file>    Specify custom config file path")
  (displayln "  --exclude <dir>    Exclude directory from analysis (can be repeated)")
  (displayln "  --parallel         Enable parallel file processing")
  (displayln "  --output <format>  Output format: text, json, sarif, junit (default: text)")
  (exit status))
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

(define (fix-require-sort lines)
  (for/list ([line (in-list lines)])
    (define trimmed (string-trim line))
    (if (regexp-match? #px"^\\(require\\s+" trimmed)
        ;; Sort require arguments
        (let* ([args-str (regexp-replace #px"^\\(require\\s+" trimmed "")]
               [args-str (regexp-replace #px"\\)\\s*$" args-str "")]
               [args (string-split args-str)]
               [sorted-args (sort args string<?)]
               [indent (make-string (string-length (car (regexp-match #px"^\\s*" line))) #\space)])
          (string-append indent "(require " (string-join sorted-args " ") ")"))
        line)))

(define (fix-provide-sort lines)
  (for/list ([line (in-list lines)])
    (define trimmed (string-trim line))
    (if (regexp-match? #px"^\\(provide\\s+" trimmed)
        ;; Sort provide arguments
        (let* ([args-str (regexp-replace #px"^\\(provide\\s+" trimmed "")]
               [args-str (regexp-replace #px"\\)\\s*$" args-str "")]
               [args (string-split args-str)]
               [sorted-args (sort args string<?)]
               [indent (make-string (string-length (car (regexp-match #px"^\\s*" line))) #\space)])
          (string-append indent "(provide " (string-join sorted-args " ") ")"))
        line)))

(define (fix-simplify-cond lines)
  (for/list ([line (in-list lines)])
    (define trimmed (string-trim line))
    ;; Replace #t with else in cond forms (on same line or next line)
    (if (or (regexp-match? #px"\\(cond\\s+\\[\\s*#t" trimmed)
            (regexp-match? #px"^\\[\\s*#t" trimmed))
        (let ([indent (make-string (string-length (car (regexp-match #px"^\\s*" line))) #\space)])
          (string-append indent (regexp-replace* #px"#t" trimmed "else")))
        line)))

;; Extract-let auto-fix: replace repeated expressions with let bindings
(define (fix-extract-let lines)
  (define expressions (make-hash))
  ;; Find all expressions in parentheses
  (for ([line (in-list lines)] [ln (in-naturals 1)])
    (define trimmed (string-trim line))
    (define exprs (regexp-match* #px"\\([^()]+\\)" trimmed))
    (for ([expr (in-list exprs)])
      (define normalized (string-trim expr))
      (when (> (string-length normalized) 5)
        (hash-update! expressions normalized
                      (lambda (old) (cons ln old))
                      '()))))
  ;; Find expressions that appear more than once
  (define repeated '())
  (for ([(expr locations) (in-hash expressions)])
    (when (>= (length locations) 2)
      (set! repeated (cons expr repeated))))
  ;; Apply fixes for repeated expressions
  (if (null? repeated)
      lines
      (let ([result lines] [counter 1])
        ;; Find the #lang line position
        (define lang-line-idx
          (for/first ([i (in-naturals)] [line (in-list lines)]
                      #:when (regexp-match? #px"^#lang" (string-trim line)))
            i))
        (for ([expr (in-list repeated)])
          (define var-name (format "_repeated~a" counter))
          (define let-line (format "(define ~a ~a)" var-name expr))
          ;; Replace all occurrences
          (set! result
                (map (lambda (line)
                       (regexp-replace* (regexp-quote expr) line var-name))
                     result))
          ;; Insert let binding after #lang line (or at beginning if no #lang)
          (if lang-line-idx
              (let ([before (take result (+ lang-line-idx 1))]
                    [after (drop result (+ lang-line-idx 1))])
                (set! result (append before (list let-line) after)))
              (set! result (cons let-line result)))
          (set! counter (+ counter 1)))
        result)))

(define (apply-fixes path diagnostics)
  (define text (call-with-input-file path port->string))
  (define lines (string-split text "\n" #:trim? #f))
  (define has-trailing? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/trailing-whitespace)) diagnostics))
  (define has-no-eof-newline? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/newline-at-eof)) diagnostics))
  (define has-require-sort? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/require-sort)) diagnostics))
  (define has-provide-sort? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/provide-sort)) diagnostics))
  (define has-simplify-cond? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/simplify-cond)) diagnostics))
  (define has-extract-let? (findf (lambda (d) (eq? (diagnostic-rule-id d) 'style/extract-let)) diagnostics))
  (define fixed-lines
    (let ([l lines])
      (when has-trailing?
        (set! l (fix-trailing-whitespace l)))
      (when has-no-eof-newline?
        (set! l (fix-newline-at-eof l)))
      (when has-require-sort?
        (set! l (fix-require-sort l)))
      (when has-provide-sort?
        (set! l (fix-provide-sort l)))
      (when has-simplify-cond?
        (set! l (fix-simplify-cond l)))
      (when has-extract-let?
        (set! l (fix-extract-let l)))
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
  (define config-file #f)
  (define exclude-dirs '())
  (define parallel? #f)
  (define output-format "text")
  (define dir #f)
  (let loop ([remaining args])
    (unless (null? remaining)
      (define arg (car remaining))
      (cond
        [(string=? arg "--fix") (set! fix? #t) (loop (cdr remaining))]
        [(string=? arg "--format") (set! format? #t) (loop (cdr remaining))]
        [(string=? arg "--no-config") (set! no-config? #t) (loop (cdr remaining))]
        [(string=? arg "--help") (usage 0)]
        [(string=? arg "--parallel") (set! parallel? #t) (loop (cdr remaining))]
        [(string=? arg "--output")
         (when (null? (cdr remaining))
           (eprintf "Error: --output requires a format (text, json, sarif, junit)\n")
           (usage))
         (set! output-format (cadr remaining))
         (loop (cddr remaining))]
        [(string=? arg "--config")
         (when (null? (cdr remaining))
           (eprintf "Error: --config requires a file path\n" )
           (usage))
         (set! config-file (cadr remaining))
         (loop (cddr remaining))]
        [(string=? arg "--exclude")
         (when (null? (cdr remaining))
           (eprintf "Error: --exclude requires a directory name\n")
           (usage))
         (set! exclude-dirs (cons (cadr remaining) exclude-dirs))
         (loop (cddr remaining))]
        [(not (string-prefix? arg "--"))
         (if dir
             (begin
               (eprintf "Error: multiple directories are not supported: ~a\n" arg)
               (usage))
             (begin
               (set! dir arg)
               (loop (cdr remaining))))]
        [else (eprintf "Unknown option: ~a\n" arg) (usage)])))
    (unless (member output-format '("text" "json" "sarif" "junit"))
      (eprintf "Error: unsupported output format: ~a\n" output-format)
      (usage))
    (values fix? format? no-config? config-file exclude-dirs parallel? output-format dir))

(define (load-user-config path)
  (with-handlers ([exn? (lambda (_) (hash))])
    (define text (call-with-input-file path port->string))
    (define body
      (regexp-replace #px"^#lang[^\n]*(\n|$)" text ""))
    (parameterize ([current-namespace (make-base-namespace)])
      (eval (read (open-input-string body))))))

(define (run args)
  (define-values (fix? format? no-config? config-file exclude-dirs parallel? output-format dir) (parse-args args))
  (unless dir (usage))
  (unless (directory-exists? dir)
    (eprintf "Error: ~a is not a directory\n" dir)
    (exit 1))
  (define files
    (let ([all-files (find-rkt-files dir)])
      (if (null? exclude-dirs)
          all-files
          (filter (lambda (f)
                    (not (ormap (lambda (exc) (string-contains? f exc)) exclude-dirs)))
                  all-files))))
  (define all-rules
    (list style/line-length style/trailing-whitespace style/newline-at-eof
          style/sexpr-depth style/definition-length style/file-length
          style/naming-convention style/require-sort style/provide-sort
          style/extract-let style/simplify-cond
          definition/unused reachability/undefined reachability/unused-require
          reachability/unused-require-expand
          export/unused module/require-provide
          abstract/type-error abstract/unreachable-code
          check-syntax/unused))
  (define user-config-file
    (if config-file
        (string->path config-file)
        (build-path dir ".racket-linter.rkt")))
  (define user-config
    (if (and (not no-config?) (file-exists? user-config-file))
        (load-user-config user-config-file)
        (hash)))
  (define default-project-config
    (hash 'module/circular-dependency (hash 'enabled #t)
          'export/unused-project (hash 'enabled #f)))
  (define merged-config
    (merge-configs default-project-config user-config))
  ;; Format files if requested
  (when format?
    (displayln "Formatting files...")
    (for ([f (in-list files)])
      (format-file f)))
  (define (run-one file)
    (with-handlers ([exn?
                     (lambda (failure)
                       (list
                        (diagnostic
                         file
                         1
                         1
                         'error
                         'linter/internal-error
                         (format "Linter rule execution failed: ~a"
                                 (exn-message failure)))))])
      (run-file all-rules merged-config file)))
  (define (run-files-parallel)
    (define channels (for/list ([file (in-list files)]) (make-channel)))
    (for ([file (in-list files)] [channel (in-list channels)])
      (thread
       (lambda ()
         (channel-put channel (run-one file)))))
    (apply append
           (for/list ([channel (in-list channels)])
             (channel-get channel))))
  ;; Run analysis concurrently when requested, while collecting diagnostics in
  ;; the same file order as sequential execution.
  (define diagnostics
    (if parallel?
        (run-files-parallel)
        (apply append (map run-one files))))
  ;; Project-level analysis respects the same rule configuration as file rules.
  (define project-diagnostics
    (filter
     (lambda (diagnostic)
       (hash-ref
        (hash-ref merged-config (diagnostic-rule-id diagnostic) (hash))
        'enabled
        #t))
     (analyze-project files)))
  (define all-diagnostics (append diagnostics project-diagnostics))
  (when fix?
    ;; Group diagnostics by file
    (define by-file (make-hash))
    (for ([d (in-list all-diagnostics)])
      (hash-update! by-file (diagnostic-path d) (lambda (old) (cons d old)) '()))
    (for ([(path diags) (in-hash by-file)])
      (apply-fixes path diags)))
  ;; Output diagnostics in the specified format
  (cond
    [(string=? output-format "json")
     (displayln (format-json-output all-diagnostics))]
    [(string=? output-format "sarif")
     (displayln (format-sarif-output all-diagnostics))]
    [(string=? output-format "junit")
     (displayln (format-junit-output all-diagnostics))]
    [else
     (print-diagnostics all-diagnostics)])
  (exit (if (null? all-diagnostics) 0 1)))

;; JSON output format
(define (diagnostic->jsexpr d)
  (hash 'path (diagnostic-path d)
        'line (diagnostic-line d)
        'col (diagnostic-col d)
        'severity (symbol->string (diagnostic-severity d))
        'rule-id (symbol->string (diagnostic-rule-id d))
        'message (diagnostic-message d)))

(define (format-json-output diagnostics)
  (jsexpr->string
   (hash 'diagnostics
         (map diagnostic->jsexpr diagnostics))))

;; SARIF output format (simplified)
(define (format-sarif-output diagnostics)
  (jsexpr->string
   (hash 'version "2.1.0"
         'runs
         (list
          (hash 'tool
                (hash 'driver
                      (hash 'name "racket-linter"
                            'version "0.2.0"))
                'results
                (map
                 (lambda (d)
                   (hash 'ruleId (symbol->string (diagnostic-rule-id d))
                         'level (symbol->string (diagnostic-severity d))
                         'message (hash 'text (diagnostic-message d))
                         'locations
                         (list
                          (hash 'physicalLocation
                                (hash 'artifactLocation
                                      (hash 'uri (diagnostic-path d))
                                      'region
                                      (hash 'startLine (diagnostic-line d)
                                            'startColumn (add1 (diagnostic-col d))))))))
                 diagnostics))))))

;; JUnit XML output format (simplified)
(define (xml-escape value)
  (regexp-replace* #rx"[&<>\"']" value
                   (lambda (match)
                     (hash-ref
                      (hash "&" "&amp;" "<" "&lt;" ">" "&gt;"
                            "\"" "&quot;" "'" "&apos;")
                      match))))

(define (format-junit-output diagnostics)
  (define num-failures (length diagnostics))
  (define results
    (for/list ([d (in-list diagnostics)])
      (define path (xml-escape (diagnostic-path d)))
      (define rule-id (xml-escape (symbol->string (diagnostic-rule-id d))))
      (define message (xml-escape (diagnostic-message d)))
      (format "<testcase name=\"~a:~a\" classname=\"~a\"><failure message=\"~a\">~a</failure></testcase>"
              path (diagnostic-line d) rule-id message message)))
  (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?><testsuite tests=\"~a\" failures=\"~a\">~a</testsuite>"
          num-failures num-failures (apply string-append results)))


(define (invoke-command)
  (run (vector->list (current-command-line-arguments))))

;; `raco` loads command modules with dynamic-require; module+ main is only
;; used for direct `racket cli/lint.rkt ...` execution.
(module+ main (invoke-command))
(when (current-command-name)
  (invoke-command))
