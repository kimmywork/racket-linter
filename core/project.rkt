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
  
  ;; Extract provide names
  (define provides
    (for/fold ([acc '()]) ([line (in-list lines)])
      (define trimmed (string-trim line))
      (cond
        ;; (provide name ...)
        ((regexp-match? #px"^\\(provide\\s+" trimmed)
         (define args (regexp-replace #px"^\\(provide\\s+" trimmed ""))
         (define args-clean (regexp-replace #px"\\)\\s*$" args ""))
         (append acc (string-split args-clean)))
        ;; (provide (all-defined-out))
        ((regexp-match? #px"^\\(provide\\s+\\(all-defined-out\\)\\)" trimmed)
         (append acc '(all-defined-out)))
        (else acc))))
  
  ;; Extract require module paths
  (define requires
    (for/fold ([acc '()]) ([line (in-list lines)])
      (define trimmed (string-trim line))
      (cond
        ;; (require module-path ...)
        ((regexp-match? #px"^\\(require\\s+" trimmed)
         (define args (regexp-replace #px"^\\(require\\s+" trimmed ""))
         (define args-clean (regexp-replace #px"\\)\\s*$" args ""))
         (define module-paths (string-split args-clean))
         ;; Filter out complex forms like (only-in ...) and (except-in ...)
         (define simple-paths
           (filter (lambda (p) (not (regexp-match? #px"^\\(" p))) module-paths))
         (append acc simple-paths))
        (else acc))))
  
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
  ;; Simple heuristic: try to find the file in the same directory
  ;; A full implementation would use Racket's module resolver
  (define dir (path-only (string->path from-path)))
  (define candidates
    (list (build-path dir (string->path (string-append require-str ".rkt")))
          (build-path dir (string->path require-str))))
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
  (define all-imports (make-hash)) ; path -> set of imported names
  
  ;; Collect all exports
  (for ([(path mod) (in-hash graph)])
    (hash-set! all-exports path (list->set (module-info-provides mod))))
  
  ;; Collect all imports (simplified: just track which modules are required)
  (for ([(path mod) (in-hash graph)])
    (hash-set! all-imports path (list->set (module-info-requires mod))))
  
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
              (define imports (hash-ref all-imports other-path (set)))
              ;; Simplified: check if the module path is imported
              ;; A full implementation would check if the specific export is used
              (set-member? imports path)))
          (when (not used?)
            (set! unused (cons (list path export) unused)))))))
  
  unused)

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
