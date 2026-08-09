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

(define (merge-configs default-config user-config)
  (for/fold ([result default-config]) ([(k v) (in-hash user-config)])
    (if (and (hash-has-key? result k) (hash? (hash-ref result k)) (hash? v))
        (hash-set result k (merge-configs (hash-ref result k) v))
        (hash-set result k v))))

;; Layer system:
;; 'text   - runs on raw text (stx is #f), always executed
;; 'syntax - runs on parsed syntax (stx is syntax?), only for safe languages
;; 'both   - runs in BOTH text and syntax phases (stx is #f then syntax?)
;; Cross-module rules need a separate dispatch mechanism beyond run-file.

(define (run-file rules config path)
  (define text (call-with-input-file path port->string))
  (define lang (detect-lang-from-text text))
  
  (define text-layer-results
    (foldl
      (lambda (rule acc)
        (define merged-config (merge-configs (rule-config-keys rule) (hash-ref config (rule-id rule) (hash))))
        (define enabled? (hash-ref merged-config 'enabled #t))
        (if (and enabled? (eq? (rule-layer rule) 'text))
            (append acc ((rule-check rule) #f path merged-config))
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
                              (define merged-config (merge-configs (rule-config-keys rule) (hash-ref config (rule-id rule) (hash))))
                              (define enabled? (hash-ref merged-config 'enabled #t))
                              (if (and enabled? (memq (rule-layer rule) '(syntax both)))
                                  (append acc ((rule-check rule) stx path merged-config))
                                  acc))
                            '()
                            rules))
                  text-layer-results))
            text-layer-results))))
