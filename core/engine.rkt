#lang racket/base

(require
  racket/string
  racket/list
  racket/match
  racket/path
  racket/function
  racket/contract/base
  racket/port
  "diagnostic.rkt"
  "rule.rkt"
  (for-syntax racket/base))

(provide
  (contract-out
    [run-file (-> (listof rule?) hash? path-string? (listof diagnostic?))]))

(define (safe-lang? lang-str)
  (member lang-str '("racket" "racket/base") string=?))

(define (detect-lang-from-text text)
  (define m (regexp-match #px"^#lang\\s+(\\S+)" text))
  (and m (second m)))

(define (read-syntax-safe path)
  (with-handlers ([exn? (lambda (e) (list (diagnostic path 1 1 'error 'read-error (exn-message e))))])
    (define text (call-with-input-file path port->string))
    (define lang (detect-lang-from-text text))
    (define text-no-lang
      (if lang
          (regexp-replace-first #px"^#lang\\s+\\S+\\s*" text "")
          text))
    (list (read-syntax path (open-input-string text-no-lang)))))

(define (regexp-replace-first pattern text replacement)
  (define positions (regexp-match-positions pattern text))
  (if positions
      (let* ([start (caar positions)]
             [end (cdar positions)]
             [before (substring text 0 start)]
             [after (substring text end)])
        (string-append before replacement after))
      text))

(define (expand-safe path stx)
  (with-handlers ([exn? (lambda (e) (list (diagnostic path 1 1 'error 'expand-error (exn-message e))))])
    (expand stx)))

(define (run-file rules config path)
  (define text (call-with-input-file path port->string))
  (define lang (detect-lang-from-text text))
  
  (define text-layer-results
    (foldl
      (lambda (rule acc)
        (define enabled? (hash-ref (rule-config-keys rule) 'enabled #t))
        (if enabled?
            (append acc ((rule-check rule) #f path))
            acc))
      '()
      rules))
  
  (if (and lang (not (safe-lang? lang)))
      text-layer-results
      (let ([stx-list (read-syntax-safe path)])
        (if (list? stx-list)
            (let ([stx (first stx-list)])
              (if (syntax? stx)
                  (append text-layer-results
                          (foldl
                            (lambda (rule acc)
                              (define enabled? (hash-ref (rule-config-keys rule) 'enabled #t))
                              (if enabled?
                                  (append acc ((rule-check rule) stx path))
                                  acc))
                            '()
                            rules))
                  text-layer-results))
            text-layer-results))))
