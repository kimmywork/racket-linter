#lang racket/base

;; Versioned diagnostic baselines. Entries use project-relative paths and an
;; exact start location plus message fingerprint so moved or changed findings
;; become visible instead of being silently ignored.

(require file/sha1
         json
         racket/contract/base
         racket/list
         racket/path
         racket/set
         racket/string
         "diagnostic.rkt")

(provide
 (struct-out baseline-entry)
 (contract-out
  [diagnostic-message-hash (-> diagnostic? string?)]
  [read-baseline
   (-> path-string? set? (values (listof baseline-entry?) (listof diagnostic?)))]
  [write-baseline! (-> path-string? path-string? (listof diagnostic?) void?)]
  [apply-baseline
   (-> (listof baseline-entry?)
       path-string?
       path-string?
       (listof diagnostic?)
       (values (listof diagnostic?) (listof diagnostic?)))]))

(struct baseline-entry (path line col rule-id message-hash) #:transparent)

(define baseline-version 1)
(define sha1-rx #px"^[0-9a-f]{40}$")

(define (diagnostic-message-hash finding)
  (sha1 (open-input-string (diagnostic-message finding))))

(define (baseline-error path message)
  (diagnostic path 1 0 'error 'baseline/invalid-file message))

(define (valid-relative-path? value)
  (and (string? value)
       (relative-path? (string->path value))
       (not (regexp-match? #px"^\\.\\.(?:/|$)" value))))

(define (entry-from-jsexpr value known-rule-ids)
  (and (hash? value)
       (let ([path (hash-ref value 'path #f)]
             [line (hash-ref value 'line #f)]
             [col (hash-ref value 'col #f)]
             [rule-id-text (hash-ref value 'rule-id #f)]
             [message-hash (hash-ref value 'message-hash #f)])
         (and (valid-relative-path? path)
              (exact-positive-integer? line)
              (exact-nonnegative-integer? col)
              (string? rule-id-text)
              (let ([rule-id (string->symbol rule-id-text)])
                (and (set-member? known-rule-ids rule-id)
                     (string? message-hash)
                     (regexp-match? sha1-rx message-hash)
                     (baseline-entry path line col rule-id message-hash)))))))

(define (read-baseline path known-rule-ids)
  (with-handlers ([exn?
                   (lambda (failure)
                     (values '()
                             (list (baseline-error
                                    path
                                    (format "Cannot read baseline: ~a"
                                            (exn-message failure))))))])
    (define value (call-with-input-file path read-json))
    (cond
      [(not (hash? value))
       (values '() (list (baseline-error path "Baseline must be a JSON object")))]
      [(not (equal? (hash-ref value 'version #f) baseline-version))
       (values '()
               (list (baseline-error
                      path
                      (format "Unsupported baseline version: ~a"
                              (hash-ref value 'version #f)))))]
      [(not (list? (hash-ref value 'diagnostics #f)))
       (values '()
               (list (baseline-error path "Baseline diagnostics must be an array")))]
      [else
       (define raw-entries (hash-ref value 'diagnostics))
       (define entries
         (map (lambda (entry) (entry-from-jsexpr entry known-rule-ids)) raw-entries))
       (if (andmap baseline-entry? entries)
           (values entries '())
           (values '()
                   (list (baseline-error
                          path
                          "Baseline contains an invalid path, location, rule ID, or message hash"))))])))

(define (->path value)
  (if (path? value) value (string->path value)))

(define (complete-path value)
  (simplify-path (path->complete-path (->path value))))

(define (diagnostic-relative-path project-root finding)
  (define root (complete-path project-root))
  (define target (complete-path (diagnostic-path finding)))
  (path->string (find-relative-path root target)))

(define (diagnostic-key project-root finding)
  (baseline-entry (diagnostic-relative-path project-root finding)
                  (diagnostic-line finding)
                  (diagnostic-col finding)
                  (diagnostic-rule-id finding)
                  (diagnostic-message-hash finding)))

(define (entry<? left right)
  (define left-key
    (list (baseline-entry-path left)
          (baseline-entry-line left)
          (baseline-entry-col left)
          (symbol->string (baseline-entry-rule-id left))
          (baseline-entry-message-hash left)))
  (define right-key
    (list (baseline-entry-path right)
          (baseline-entry-line right)
          (baseline-entry-col right)
          (symbol->string (baseline-entry-rule-id right))
          (baseline-entry-message-hash right)))
  (let loop ([left-parts left-key] [right-parts right-key])
    (cond
      [(null? left-parts) #f]
      [(equal? (first left-parts) (first right-parts))
       (loop (rest left-parts) (rest right-parts))]
      [(and (number? (first left-parts)) (number? (first right-parts)))
       (< (first left-parts) (first right-parts))]
      [else (string<? (first left-parts) (first right-parts))])))

(define (write-baseline! path project-root diagnostics)
  (define entries
    (sort (remove-duplicates
           (map (lambda (finding) (diagnostic-key project-root finding)) diagnostics))
          entry<?))
  (define value
    (hash 'version baseline-version
          'diagnostics
          (for/list ([entry (in-list entries)])
            (hash 'path (baseline-entry-path entry)
                  'line (baseline-entry-line entry)
                  'col (baseline-entry-col entry)
                  'rule-id (symbol->string (baseline-entry-rule-id entry))
                  'message-hash (baseline-entry-message-hash entry)))))
  (call-with-output-file path
    (lambda (out)
      (write-json value out)
      (newline out))
    #:exists 'replace))

(define (apply-baseline entries baseline-path project-root diagnostics)
  (define current-keys
    (for/set ([finding (in-list diagnostics)])
      (diagnostic-key project-root finding)))
  (define baseline-keys (list->set entries))
  (define remaining
    (filter (lambda (finding)
              (not (set-member? baseline-keys
                                (diagnostic-key project-root finding))))
            diagnostics))
  (define stale
    (for/list ([entry (in-list entries)]
               #:unless (set-member? current-keys entry))
      (diagnostic
       baseline-path 1 0 'warning 'baseline/stale-entry
       (format "Stale baseline entry: ~a:~a:~a ~a"
               (baseline-entry-path entry)
               (baseline-entry-line entry)
               (baseline-entry-col entry)
               (baseline-entry-rule-id entry)))))
  (values remaining stale))
