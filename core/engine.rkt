#lang racket/base

;; Engine for text, syntax, and expanded-syntax rule phases.

(require
  racket/string
  racket/list
  racket/path
  racket/contract/base
  racket/port
  syntax/modread
  "diagnostic.rkt"
  "rule.rkt")

(provide
 (contract-out
  [run-file (-> (listof rule?) hash? path-string? (listof diagnostic?))])
 merge-configs)

(define (safe-lang? lang-str)
  (member lang-str
          '("racket" "racket/base" "racket/contract" "racket/contract/base"
            "racket/class" "racket/date" "racket/dict" "racket/function"
            "racket/list" "racket/match" "racket/math" "racket/port"
            "racket/pretty" "racket/require" "racket/set" "racket/string"
            "racket/vector" "racket/format" "racket/gui" "racket/gui/base"
            "racket/future" "racket/flonum" "racket/fixnum" "racket/unsafe/ops")
          string=?))

(define (detect-lang-from-text text)
  (define match (regexp-match #px"^#lang\\s+(\\S+)" text))
  (and match (second match)))

(define (regexp-replace-first pattern text replacement)
  (define positions (regexp-match-positions pattern text))
  (if positions
      (let* ([start (caar positions)]
             [end (cdar positions)])
        (string-append (substring text 0 start)
                       replacement
                       (substring text end)))
      text))

(define (read-forms path)
  (define text (call-with-input-file path port->string))
  (define lang (detect-lang-from-text text))
  (define source
    (if lang
        (regexp-replace-first #px"^#lang\\s+\\S+\\s*" text "")
        text))
  (define in (open-input-string source))
  (let loop ([forms '()])
    (define stx (read-syntax path in))
    (if (eof-object? stx)
        (reverse forms)
        (loop (cons stx forms)))))

(define (read-syntax-safe path)
  (with-handlers ([exn?
                   (lambda (exn)
                     (list (diagnostic path 1 0 'error 'read-error
                                       (exn-message exn))))])
    (define text (call-with-input-file path port->string))
    (define lang (detect-lang-from-text text))
    (if lang
        (list
         (call-with-input-file path
           (lambda (in)
             (port-count-lines! in)
             (with-module-reading-parameterization
              (lambda () (read-syntax path in))))))
        (let ([in (open-input-string text)])
          (port-count-lines! in)
          (define forms
            (let loop ([result '()])
              (define stx (read-syntax path in))
              (if (eof-object? stx)
                  (reverse result)
                  (loop (cons stx result)))))
          (cond
            [(null? forms) '()]
            [(null? (cdr forms)) (list (first forms))]
            [else
             (list (datum->syntax #f
                                  (cons (datum->syntax #f 'begin) forms)
                                  (vector path 1 0 1 1))) ])))))

(define (read-syntax-all path)
  (with-handlers ([exn?
                   (lambda (exn)
                     (list (diagnostic path 1 0 'error 'read-error
                                       (exn-message exn))))])
    (call-with-input-file path
      (lambda (in)
        (port-count-lines! in)
        (with-module-reading-parameterization
         (lambda () (read-syntax path in)))))))

(define (expand-safe path stx)
  (with-handlers ([exn?
                   (lambda (exn)
                     (list (diagnostic path 1 0 'error 'expand-error
                                       (exn-message exn))))])
    (parameterize ([current-namespace (make-base-namespace)]
                   [current-load-relative-directory
                    (path-only (path->complete-path (string->path path)))])
      (expand stx))))

(define (merge-configs default-config user-config)
  (for/fold ([result default-config]) ([(key value) (in-hash user-config)])
    (if (and (hash-has-key? result key)
             (hash? (hash-ref result key))
             (hash? value))
        (hash-set result key
                  (merge-configs (hash-ref result key) value))
        (hash-set result key value))))

(define (rule-config rule config)
  (merge-configs (rule-config-keys rule)
                 (hash-ref config (rule-id rule) (hash))))

(define (rule-enabled? rule config)
  (hash-ref (rule-config rule config) 'enabled #t))

(define (run-text-rules rules config path)
  (for/fold ([result '()]) ([rule (in-list rules)]
                            #:when (and (rule-enabled? rule config)
                                        (eq? (rule-layer rule) 'text)))
    (append result
            ((rule-check rule) #f path (rule-config rule config)))))

(define (run-syntax-rules rules config stx path)
  (for/fold ([result '()]) ([rule (in-list rules)]
                            #:when (and (rule-enabled? rule config)
                                        (memq (rule-layer rule) '(syntax both))))
    (append result
            ((rule-check rule) stx path (rule-config rule config)))))

(define (run-expand-rules rules config expanded path)
  (for/fold ([result '()]) ([rule (in-list rules)]
                            #:when (and (rule-enabled? rule config)
                                        (eq? (rule-layer rule) 'expand)))
    (append result
            ((rule-check rule) expanded path (rule-config rule config)))))

;; Text diagnostics always run. Safe languages additionally get parsed and,
;; when requested by a rule, expanded. Read/expand errors are returned as
;; diagnostics instead of being converted into an empty result.
(define (run-file rules config path)
  (define text-results (run-text-rules rules config path))
  (define text (call-with-input-file path port->string))
  (define lang (detect-lang-from-text text))
  (cond
    [(and lang (not (safe-lang? lang))) text-results]
    [else
     (define syntax-result (read-syntax-safe path))
     (cond
       [(and (pair? syntax-result)
             (diagnostic? (first syntax-result)))
        (append text-results syntax-result)]
       [(null? syntax-result) text-results]
       [else
        (define stx (first syntax-result))
        (define syntax-results (run-syntax-rules rules config stx path))
        (define expand-results
          (if (ormap (lambda (rule)
                       (and (rule-enabled? rule config)
                            (eq? (rule-layer rule) 'expand)))
                     rules)
              (let ([expanded (expand-safe path (read-syntax-all path))])
                (if (syntax? expanded)
                    (run-expand-rules rules config expanded path)
                    expanded))
              '()))
        (append text-results syntax-results expand-results)])]))
