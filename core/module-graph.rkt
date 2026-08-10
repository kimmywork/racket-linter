#lang racket/base

;; Source-preserving module graph facts. This is intentionally separate from
;; the legacy regex project graph so existing callers keep their contract while
;; phase-aware analysis can grow independently.

(require
  racket/list
  racket/path
  racket/port
  racket/string
  syntax/modread
  "diagnostic.rkt")

(provide
 (struct-out require-edge)
 (struct-out module-facts)
 parse-module-facts
 build-phase-module-graph
 check-phase-module-graph)

(struct require-edge
  (source raw module-path phase imported-names line col)
  #:transparent)
(struct module-facts
  (path definitions provides requires errors)
  #:transparent)

(define (parts-of stx)
  (define parts (and (syntax? stx) (syntax->list stx)))
  (and (pair? parts) parts))

(define (head-name parts)
  (and (pair? parts) (identifier? (first parts)) (syntax-e (first parts))))

(define (syntax-name stx)
  (and (identifier? stx) (syntax-e stx)))

(define (read-module path)
  (call-with-input-file path
    (lambda (in)
      (port-count-lines! in)
      (with-module-reading-parameterization
       (lambda () (read-syntax path in))))))

(define (source-location stx)
  (values (or (syntax-line stx) 1) (or (syntax-column stx) 0)))

(define (module-path-value stx)
  (define value (syntax-e stx))
  (cond
    [(symbol? value) (symbol->string value)]
    [(string? value) value]
    [else #f]))

(define (phase-wrapper? name)
  (memq name '(for-syntax for-template for-label for-meta)))

(define (phase-name name)
  (case name
    [(for-syntax) 'for-syntax]
    [(for-template) 'for-template]
    [(for-label) 'for-label]
    [(for-meta) 'for-meta]
    [else 'normal]))

(define (require-edge-from-spec source spec phase)
  (define parts (parts-of spec))
  (define line-col (call-with-values (lambda () (source-location spec)) list))
  (define line (first line-col))
  (define col (second line-col))
  (cond
    [(not parts)
     (define module-path (module-path-value spec))
     (and module-path
          (require-edge source spec module-path phase '() line col))]
    [(phase-wrapper? (head-name parts))
     (append-map (lambda (nested)
                   (or (let ([edge (require-edge-from-spec source nested
                                                            (phase-name (head-name parts)))])
                         (if (list? edge) edge (list edge)))
                       '()))
                 (rest parts))]
    [(eq? (head-name parts) 'only-in)
     (define base (and (> (length parts) 1)
                       (require-edge-from-spec source (second parts) phase)))
     (and base
          (struct-copy require-edge base
                       [imported-names
                        (for/list ([name (in-list (drop parts 2))]
                                   #:when (identifier? name))
                          (syntax-name name))]))]
    [(eq? (head-name parts) 'except-in)
     (define base (and (> (length parts) 1)
                       (require-edge-from-spec source (second parts) phase)))
     (and base
          (struct-copy require-edge base
                       [imported-names
                        (for/list ([name (in-list (drop parts 2))]
                                   #:when (identifier? name))
                          (string->symbol (format "except ~a" (syntax-name name))))]))]
    [(eq? (head-name parts) 'prefix-in)
     (and (> (length parts) 2)
          (require-edge-from-spec source (third parts) phase))]
    [(eq? (head-name parts) 'rename-in)
     (define base (and (> (length parts) 1)
                       (require-edge-from-spec source (second parts) phase)))
     (and base (struct-copy require-edge base
                            [imported-names
                             (for/list ([mapping (in-list (drop parts 2))]
                                        #:when (parts-of mapping))
                               (define mapping-parts (parts-of mapping))
                               (and (pair? mapping-parts)
                                    (syntax-name (first mapping-parts))))]))]
    [(eq? (head-name parts) 'submod)
     (and (> (length parts) 1)
          (let ([base (require-edge-from-spec source (second parts) phase)])
            (and base
                 (struct-copy require-edge base
                              [module-path
                               (string-append (require-edge-module-path base)
                                              "#submod/")]))))]
    [(eq? (head-name parts) 'file)
     (and (> (length parts) 1)
          (require-edge-from-spec source (second parts) phase))]
    [else #f]))

(define (collect-definition-names stx)
  (define parts (parts-of stx))
  (cond
    [(not parts) '()]
    [(eq? (head-name parts) 'define-values)
     (filter-map syntax-name (or (parts-of (second parts)) '()))]
    [(eq? (head-name parts) 'define)
     (define target (second parts))
     (list (or (syntax-name target)
               (and (parts-of target)
                    (syntax-name (first (parts-of target))))))]
    [(eq? (head-name parts) 'struct)
     (if (and (> (length parts) 1) (identifier? (second parts)))
         (list (syntax-name (second parts)))
         '())]
    [else '()]))

(define (collect-provide-names stx)
  (define parts (parts-of stx))
  (cond
    [(identifier? stx) (list (syntax-name stx))]
    [(not parts) '()]
    [(eq? (head-name parts) 'all-defined-out) '(all-defined-out)]
    [(eq? (head-name parts) 'rename-out)
     (for/list ([mapping (in-list (rest parts))]
                #:when (parts-of mapping))
       (define mapping-parts (parts-of mapping))
       (and (pair? mapping-parts)
            (syntax-name (second mapping-parts))))]
    [(eq? (head-name parts) 'struct-out)
     (if (> (length parts) 1) (list (syntax-name (second parts))) '())]
    [(eq? (head-name parts) 'contract-out)
     (for/list ([entry (in-list (rest parts))]
                #:when (parts-of entry))
       (define entry-parts (parts-of entry))
       (and (pair? entry-parts) (syntax-name (first entry-parts))))]
    [else '()]))

(define (parse-module-facts path)
  (with-handlers ([exn?
                   (lambda (exn)
                     (module-facts path '() '() '() (list (exn-message exn))))])
    (define module-stx (read-module path))
    (define definitions '())
    (define provides '())
    (define requires '())
    (define (walk stx)
      (define parts (parts-of stx))
      (when parts
        (case (head-name parts)
          [(require)
           (for ([spec (in-list (rest parts))])
             (define edge (require-edge-from-spec path spec 'normal))
             (cond
               [(require-edge? edge) (set! requires (cons edge requires))]
               [(list? edge) (set! requires (append edge requires))]))]
          [(provide)
           (set! provides
                 (append (append-map collect-provide-names (rest parts))
                         provides))]
          [else
           (set! definitions
                 (append (collect-definition-names stx) definitions))
           (for ([child (in-list (rest parts))])
             (unless (memq (head-name parts) '(quote quasiquote syntax))
               (walk child)))])))
    (walk module-stx)
    (module-facts path
                  (filter values (remove-duplicates definitions))
                  (filter values (remove-duplicates provides))
                  (reverse (flatten requires))
                  '())))

(define (build-phase-module-graph files)
  (for/hash ([path (in-list files)])
    (define complete
      (path->string (simplify-path (path->complete-path (string->path path)))))
    (values complete (parse-module-facts complete))))

(define (relative-edge? edge)
  (string? (syntax-e (require-edge-raw edge))))

(define (resolve-edge edge)
  (define raw (require-edge-module-path edge))
  (if (not (relative-edge? edge))
      #f
      (let* ([source-dir (path-only (path->complete-path
                                     (string->path (require-edge-source edge))))]
             [base (build-path source-dir (string->path raw))]
             [candidates (list base
                               (string->path (string-append (path->string base) ".rkt"))
                               (build-path base "main.rkt"))])
        (for/first ([candidate (in-list candidates)]
                    #:when (file-exists? candidate))
          (path->string (simplify-path candidate))))))

(define (check-phase-module-graph files)
  (define graph (build-phase-module-graph files))
  (define diagnostics '())
  (define cycle-seen (make-hash))
  (define (walk path phase stack)
    (define key (list path phase))
    (if (member key stack)
        (unless (hash-has-key? cycle-seen key)
          (hash-set! cycle-seen key #t)
          (set! diagnostics
                (cons (diagnostic path 1 0 'error 'module/phase-cycle
                                  (format "Circular dependency at phase ~a: ~a"
                                          phase path))
                      diagnostics)))
        (when (hash-has-key? graph path)
          (for ([edge (in-list (module-facts-requires (hash-ref graph path)))])
            (define target (resolve-edge edge))
            (when (and target (hash-has-key? graph target))
              (walk target (require-edge-phase edge) (cons key stack)))))))
  (for ([path (in-list files)])
    (walk (path->string (simplify-path (path->complete-path (string->path path))))
          'normal
          '()))
  (for ([(path facts) (in-hash graph)])
    (for ([message (in-list (module-facts-errors facts))])
      (set! diagnostics
            (cons (diagnostic path 1 0 'error 'module/phase-parse message)
                  diagnostics)))
    (for ([edge (in-list (module-facts-requires facts))])
      (when (and (relative-edge? edge)
                 (not (resolve-edge edge)))
        (set! diagnostics
              (cons (diagnostic (require-edge-source edge)
                                (require-edge-line edge)
                                (require-edge-col edge)
                                'warning 'module/phase-unresolved-require
                                (format "Cannot resolve ~a at phase ~a"
                                        (require-edge-module-path edge)
                                        (require-edge-phase edge)))
                    diagnostics)))))
  diagnostics)
