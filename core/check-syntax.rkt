#lang racket/base

;; Precise binding facts collected from DrRacket's check-syntax traversal.
;; The traversal is used as a semantic supplement to the local syntax walker:
;; it knows lexical binding identity, phase-aware references, and require use.

(require
  drracket/check-syntax
  racket/class
  racket/contract/base
  racket/list
  racket/path
  racket/port
  racket/string
  syntax/modread
  "diagnostic.rkt")

(provide
 (contract-out
  [struct syntax-span
          ([source path-string?]
           [start exact-nonnegative-integer?]
           [end exact-nonnegative-integer?]
           [name any/c])]
  [struct syntax-reference
          ([source path-string?]
           [start exact-nonnegative-integer?]
           [end exact-nonnegative-integer?]
           [name any/c]
           [definition-source any/c]
           [definition-start (or/c #f exact-nonnegative-integer?)]
           [definition-end (or/c #f exact-nonnegative-integer?)])]
  [struct syntax-facts
          ([definitions (listof syntax-span?)]
           [references (listof syntax-reference?)]
           [unused-binders (listof syntax-span?)]
           [unused-requires (listof syntax-span?)]
           [errors (listof string?)])]
  [check-syntax-facts (-> path-string? syntax-facts?)]
  [check-syntax-analyze (-> path-string? (listof diagnostic?))]))

(struct syntax-span (source start end name) #:transparent)
(struct syntax-reference
  (source start end name definition-source definition-start definition-end)
  #:transparent)
(struct syntax-facts (definitions references unused-binders unused-requires errors)
  #:transparent)

(define (same-source? expected actual)
  (and actual
       (equal? (path->string (simplify-path (string->path expected)))
               (path->string (simplify-path
                              (if (path? actual) actual (string->path actual)))))))

(define (collector%/for path)
  (class (annotations-mixin object%)
    (init-field source-path)
    (define definitions '())
    (define references '())
    (define unused-binders '())
    (define unused-requires '())
    (super-new)

    ;; Returning the path makes every callback directly attributable to the
    ;; source file and excludes expansion-only syntax from the fact set.
    (define/override (syncheck:find-source-object stx)
      (and (same-source? source-path (syntax-source stx)) source-path))

    (define/public (syncheck:add-definition-source source start end name)
      (set! definitions (cons (syntax-span source-path start end name) definitions)))

    (define/override (syncheck:add-definition-target source start end name mods)
      (set! definitions (cons (syntax-span source-path start end name) definitions)))

    (define/override (syncheck:add-jump-to-definition source start end name
                                                       definition-source
                                                       submodules)
      (set! references
            (cons (syntax-reference source-path start end name
                                    (list definition-source submodules)
                                    #f #f)
                  references)))

    (define/override (syncheck:unused-binder source start end)
      (set! unused-binders
            (cons (syntax-span source-path start end #f) unused-binders)))

    (define/override (syncheck:add-unused-require source start end)
      (set! unused-requires
            (cons (syntax-span source-path start end #f) unused-requires)))

    (define/public (facts)
      (syntax-facts (reverse definitions)
                    (reverse references)
                    (reverse unused-binders)
                    (reverse unused-requires)
                    '()))))

(define (read-module-syntax path)
  (call-with-input-file path
    (lambda (in)
      (port-count-lines! in)
      (with-module-reading-parameterization
       (lambda () (read-syntax path in))))))

(define (check-syntax-facts path)
  (define collector (new (collector%/for path) [source-path path]))
  (define errors '())
  (define source-dir
    (path-only (path->complete-path (string->path path))))
  (define ns (make-base-namespace))
  (with-handlers ([exn:fail?
                   (lambda (exn)
                     (set! errors (list (exn-message exn))))])
    (define stx
      (parameterize ([current-load-relative-directory source-dir]
                     [current-namespace ns])
        (read-module-syntax path)))
    (unless (eof-object? stx)
      (define-values (add-syntax done)
        (make-traversal ns source-dir))
      (parameterize ([current-load-relative-directory source-dir]
                     [current-namespace ns]
                     [current-annotations collector]
                     [current-output-port (open-output-nowhere)])
        (add-syntax (expand stx))
        (done))))
  (define facts (send collector facts))
  (struct-copy syntax-facts facts [errors errors]))

(define (source-position path position)
  (define text (call-with-input-file path port->string))
  (define bounded (min position (string-length text)))
  (define line (add1 (for/sum ([ch (in-string (substring text 0 bounded))]
                               #:when (char=? ch #\newline))
                      1)))
  (define last-newline
    (for/fold ([result -1]) ([idx (in-range bounded)]
                             #:when (char=? (string-ref text idx) #\newline))
      idx))
  (values line (max 0 (- bounded last-newline 1))))

(define (span-diagnostic path span rule-id message)
  (define-values (line col) (source-position path (syntax-span-start span)))
  (diagnostic path line col 'info rule-id message))

(define (check-syntax-analyze path)
  (define facts (check-syntax-facts path))
  (append
   (for/list ([message (in-list (syntax-facts-errors facts))])
     (diagnostic path 1 0 'error 'check-syntax/error message))
   (for/list ([span (in-list (syntax-facts-unused-binders facts))])
     (span-diagnostic path span 'check-syntax/unused-variable
                      "binding is defined but never used"))
   (for/list ([span (in-list (syntax-facts-unused-requires facts))])
     (span-diagnostic path span 'check-syntax/unused-require
                      "require is not used by this module"))))
