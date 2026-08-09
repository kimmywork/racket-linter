#lang racket/base

(require
  racket/list
  racket/string
  racket/set
  racket/hash
  racket/path
  racket/port
  racket/contract/base
  "diagnostic.rkt"
  "rule.rkt")

(provide
  (contract-out
    [analyze-project (-> (listof path-string?) (listof diagnostic?))])
  build-dependency-graph
  find-circular-dependencies
  find-unused-exports)

;; Module info: path, provides, requires
(struct module-info (path provides requires) #:transparent)

;; Parse a file to extract provide and require forms
(define (parse-module-info path)
  (define text (call-with-input-file path port->string))
  (define lines (string-split text "\n"))
  
  ;; Extract provide names (handle multi-line forms)
  (define provides
    (let loop ([lines lines] [acc '()] [in-provide? #f] [provide-text ""])
      (cond
        [(null? lines) acc]
        [else
         (define trimmed (string-trim (car lines)))
         (cond
           ;; Start of provide form
           [(and (not in-provide?) (regexp-match? #px"^\\(provide\\s+" trimmed))
            (if (regexp-match? #px"\\)\\s*$" trimmed)
                ;; Single-line provide
                (let* ([args (regexp-replace #px"^\\(provide\\s+" trimmed "")]
                       [args-clean (regexp-replace #px"\\)\\s*$" args "")])
                  (loop (cdr lines) (append acc (string-split args-clean)) #f ""))
                ;; Multi-line provide
                (loop (cdr lines) acc #t trimmed))]
           ;; Continuation of multi-line provide
           [(and in-provide? (not (regexp-match? #px"\\)\\s*$" trimmed)))
            (loop (cdr lines) acc #t (string-append provide-text " " trimmed))]
           ;; End of multi-line provide
           [(and in-provide? (regexp-match? #px"\\)\\s*$" trimmed))
            (define full-text (string-append provide-text " " trimmed))
            (define args (regexp-replace #px"^\\(provide\\s+" full-text ""))
            (define args-clean (regexp-replace #px"\\)\\s*$" args ""))
            (loop (cdr lines) (append acc (string-split args-clean)) #f "")]
           ;; Not in provide
           [else (loop (cdr lines) acc #f "")])])))
  
  ;; Extract require module paths (handle multi-line forms)
  (define requires
    (let loop ([lines lines] [acc '()] [in-require? #f] [require-text ""])
      (cond
        [(null? lines) acc]
        [else
         (define trimmed (string-trim (car lines)))
         (cond
           ;; Start of require form
           [(and (not in-require?) (regexp-match? #px"^\\(require\\s+" trimmed))
            (if (regexp-match? #px"\\)\\s*$" trimmed)
                ;; Single-line require
                (let* ([args (regexp-replace #px"^\\(require\\s+" trimmed "")]
                       [args-clean (regexp-replace #px"\\)\\s*$" args "")]
                       [module-paths (string-split args-clean)]
                       [simple-paths (filter (lambda (p) (not (regexp-match? #px"^\\(" p))) module-paths)])
                  (loop (cdr lines) (append acc simple-paths) #f ""))
                ;; Multi-line require
                (loop (cdr lines) acc #t trimmed))]
           ;; Continuation of multi-line require
           [(and in-require? (not (regexp-match? #px"\\)\\s*$" trimmed)))
            (loop (cdr lines) acc #t (string-append require-text " " trimmed))]
           ;; End of multi-line require
           [(and in-require? (regexp-match? #px"\\)\\s*$" trimmed))
            (define full-text (string-append require-text " " trimmed))
            (define args (regexp-replace #px"^\\(require\\s+" full-text ""))
            (define args-clean (regexp-replace #px"\\)\\s*$" args ""))
            (define module-paths (string-split args-clean))
            (define simple-paths (filter (lambda (p) (not (regexp-match? #px"^\\(" p))) module-paths))
            (loop (cdr lines) (append acc simple-paths) #f "")]
           ;; Not in require
           [else (loop (cdr lines) acc #f "")])])))
  
  (module-info path provides requires))

;; Build dependency graph from a list of files
(define (build-dependency-graph files)
  (define modules (map parse-module-info files))
  ;; Build a hash: path -> module-info
  (for/hash ([m (in-list modules)])
    (values (module-info-path m) m)))

;; Find circular dependencies using DFS
(define (find-circular-dependencies graph)
  (define visited (make-hash))
  (define in-stack (make-hash))
  (define cycles '())
  
  (define (dfs path)
    (when (not (hash-has-key? visited path))
      (hash-set! visited path #t)
      (hash-set! in-stack path #t)
      
      (define mod (hash-ref graph path #f))
      (when mod
        (for ([req (in-list (module-info-requires mod))])
          ;; Resolve require to a file path
          (define req-path (resolve-require path req))
          (when req-path
            (cond
              ((hash-has-key? in-stack req-path)
               ;; Found a cycle
               (set! cycles (cons (list req-path path) cycles)))
              (else
               (dfs req-path))))))
      
      (hash-remove! in-stack path)))
  
  ;; Run DFS from each node
  (for ([path (in-hash-keys graph)])
    (dfs path))
  
  cycles)

;; Resolve a require path to an absolute file path
(define (resolve-require from-path require-str)
  ;; Try multiple resolution strategies:
  ;; 1. Same directory (relative path)
  ;; 2. Parent directory (for nested modules)
  ;; 3. Project root (for absolute paths)
  (define dir (path-only (string->path from-path)))
  (define candidates
    (list 
      ;; Same directory
      (build-path dir (string->path (string-append require-str ".rkt")))
      (build-path dir (string->path require-str))
      ;; Parent directory (for nested modules like "../core/rule.rkt")
      (build-path (simplify-path (build-path dir "..")) 
                  (string->path (string-append require-str ".rkt")))
      (build-path (simplify-path (build-path dir ".."))
                  (string->path require-str))
      ;; Two levels up
      (build-path (simplify-path (build-path dir ".." ".."))
                  (string->path (string-append require-str ".rkt")))
      (build-path (simplify-path (build-path dir ".." ".."))
                  (string->path require-str))))
  (for/first ([c (in-list candidates)]
               #:when (file-exists? c))
    (path->string c)))

;; Find exports that are not used anywhere in the project
(define (find-unused-exports graph)
  ;; Common public API names that should not be flagged
  (define public-api-names
    (list "run" "main" "start" "stop" "init" "reset"
          "provide" "require" "module" "module+"
          "info" "version" "help"))
  
  (define all-exports (make-hash)) ; path -> set of exported names
  (define all-imports (make-hash)) ; path -> set of (module-path, name) pairs
  
  ;; Collect all exports
  (for ([(path mod) (in-hash graph)])
    (hash-set! all-exports path (list->set (module-info-provides mod))))
  
  ;; Collect all imports with specific names
  (for ([(path mod) (in-hash graph)])
    (define imports (make-hash)) ; module-path -> set of imported names
    (for ([req (in-list (module-info-requires mod))])
      ;; Parse require form to extract specific names
      (define names (parse-require-names req))
      (when (not (null? names))
        (hash-update! imports req (lambda (old) (set-union old (list->set names))) (set))))
    (hash-set! all-imports path imports))
  
  ;; Find exports that are not imported by any other module
  (define unused '())
  (for ([(path exports) (in-hash all-exports)])
    (when (not (set-member? exports 'all-defined-out))
      (for ([export (in-set exports)])
        ;; Skip public API names
        (when (not (member export public-api-names))
          (define used?
            (for/or ([other-path (in-hash-keys graph)]
                     #:when (not (string=? other-path path)))
              (define imports (hash-ref all-imports other-path (hash)))
              ;; Check if the module is imported AND the specific export is used
              (for/or ([(mod-path names) (in-hash imports)])
                (and (set-member? names export) #t))))
          (when (not used?)
            (set! unused (cons (list path export) unused)))))))
  
  unused)

;; Parse require form to extract specific imported names
(define (parse-require-names require-str)
  ;; Handle (only-in mod name ...) and (rename-in mod [old new] ...)
  (cond
    ((regexp-match? #px"^\\(only-in\\s+" require-str)
     ;; Extract names from (only-in mod name ...)
     (define parts (string-split (regexp-replace #px"^\\(only-in\\s+" require-str "")))
     (if (>= (length parts) 2)
         (cdr parts) ; Skip module path
         '()))
    ((regexp-match? #px"^\\(rename-in\\s+" require-str)
     ;; Extract names from (rename-in mod [old new] ...)
     (define parts (string-split (regexp-replace #px"^\\(rename-in\\s+" require-str "")))
     (if (>= (length parts) 2)
         (filter (lambda (p) (not (string=? p "["))) (cdr parts))
         '()))
    (else
     ;; Simple module path - no specific names
     '())))

;; Main analysis function
(define (analyze-project files)
  (define graph (build-dependency-graph files))
  (define diagnostics '())
  
  ;; Find circular dependencies
  (define cycles (find-circular-dependencies graph))
  (for ([cycle (in-list cycles)])
    (set! diagnostics
          (cons (diagnostic (first cycle) 1 1 'error 'module/circular-dependency
                           (format "Circular dependency detected: ~a -> ~a"
                                   (first cycle) (second cycle)))
                diagnostics)))
  
  ;; Find unused exports
  (define unused (find-unused-exports graph))
  (for ([u (in-list unused)])
    (set! diagnostics
          (cons (diagnostic (first u) 1 1 'info 'export/unused-project
                           (format "Export ~a is not used by any other module in the project"
                                   (second u)))
                diagnostics)))
  
  diagnostics)
