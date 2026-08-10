#lang racket/base

;; Source suppression directives are intentionally rule-specific. A directive
;; only affects diagnostics whose starting line is covered by the directive.

(require racket/contract/base
         racket/list
         racket/port
         racket/set
         racket/string
         "diagnostic.rkt")

(provide
 (struct-out suppression-index)
 (contract-out
  [read-suppressions
   (-> path-string? set? (values suppression-index? (listof diagnostic?)))]
  [diagnostic-suppressed? (-> suppression-index? diagnostic? boolean?)]))

(struct suppression-index (line-rules) #:transparent)

(define directive-rx
  #px"^\\s*;+\\s*racket-linter-(disable-next-line|disable-line|disable|enable)(?:\\s+(.*?))?\\s*$")
(define directive-prefix-rx #px"^\\s*;+\\s*racket-linter-")

(define (directive-diagnostic path line rule-id message)
  (diagnostic path line 0 'error rule-id message))

(define (parse-rule-ids text)
  (if (and text (not (string=? (string-trim text) "")))
      (remove-duplicates
       (map string->symbol (string-split (string-trim text))))
      '()))

(define (read-suppressions path known-rule-ids)
  (define lines
    (string-split (call-with-input-file path port->string) "\n" #:trim? #f))
  (define line-rules (make-hash))
  (define active (set))
  (define diagnostics '())

  (define (report! line rule-id message)
    (set! diagnostics
          (cons (directive-diagnostic path line rule-id message) diagnostics)))

  (for ([line-text (in-list lines)] [line (in-naturals 1)])
    (define match (regexp-match directive-rx line-text))
    (cond
      [match
       (define action (string->symbol (second match)))
       (define rule-ids (parse-rule-ids (third match)))
       (define unknown
         (filter (lambda (rule-id) (not (set-member? known-rule-ids rule-id)))
                 rule-ids))
       (cond
         [(null? rule-ids)
          (report! line 'suppression/invalid-directive
                   "Suppression directive must name at least one rule ID")]
         [(pair? unknown)
          (report! line 'suppression/unknown-rule
                   (format "Unknown suppression rule ID~a: ~a"
                           (if (= (length unknown) 1) "" "s")
                           (string-join (map symbol->string unknown) ", ")))]
         [else
          (case action
            [(disable)
             (set! active (set-union active (list->set rule-ids)))]
            [(enable)
             (define inactive
               (filter (lambda (rule-id) (not (set-member? active rule-id)))
                       rule-ids))
             (if (pair? inactive)
                 (report! line 'suppression/unmatched-enable
                          (format "Cannot enable rule~a that ~a not disabled: ~a"
                                  (if (= (length inactive) 1) "" "s")
                                  (if (= (length inactive) 1) "is" "are")
                                  (string-join (map symbol->string inactive) ", ")))
                 (set! active
                       (for/fold ([current active]) ([rule-id (in-list rule-ids)])
                         (set-remove current rule-id))))]
            [(disable-line)
             (hash-update! line-rules line
                           (lambda (current)
                             (set-union current (list->set rule-ids)))
                           (set))]
            [(disable-next-line)
             (hash-update! line-rules (add1 line)
                           (lambda (current)
                             (set-union current (list->set rule-ids)))
                           (set))])])]
      [(regexp-match? directive-prefix-rx line-text)
       (report! line 'suppression/invalid-directive
                "Invalid racket-linter suppression directive")])
    (unless (set-empty? active)
      (hash-update! line-rules line
                    (lambda (current) (set-union current active))
                    (set))))

  (values (suppression-index
           (for/hash ([(line rules) (in-hash line-rules)])
             (values line rules)))
          (reverse diagnostics)))

(define (diagnostic-suppressed? index finding)
  (set-member?
   (hash-ref (suppression-index-line-rules index)
             (diagnostic-line finding)
             (set))
   (diagnostic-rule-id finding)))
