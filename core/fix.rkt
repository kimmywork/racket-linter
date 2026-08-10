#lang racket/base

;; Conservative source fixes. Every candidate has a source applicability check,
;; a replacement preview, and a second-pass no-op/idempotence check.

(require racket/contract/base
         racket/list
         racket/port
         racket/string
         syntax/modread
         "diagnostic.rkt")

(provide
 (struct-out fix-edit)
 (struct-out fix-result)
 (contract-out
  [safe-fix-rule? (-> symbol? boolean?)]
  [apply-safe-fixes (-> path-string? string? (listof diagnostic?) fix-result?)]))

(struct fix-edit (start end replacement rule-id line col before) #:transparent)
(struct fix-result (text changed? applicable? reason edits) #:transparent)

(define safe-fix-rules
  '(style/trailing-whitespace style/newline-at-eof style/simplify-cond))

(define (safe-fix-rule? rule-id)
  (and (memq rule-id safe-fix-rules) #t))

(define (diagnostic-for? diagnostics rule-id line [col #f])
  (ormap (lambda (finding)
           (and (eq? (diagnostic-rule-id finding) rule-id)
                (= (diagnostic-line finding) line)
                (or (not col) (= (diagnostic-col finding) col))))
         diagnostics))

(define (line-ending-edit text diagnostics)
  (define lines (string-split text "\n" #:trim? #f))
  (define edits '())
  (define offset 0)
  (for ([line (in-list lines)] [line-number (in-naturals 1)])
    (define content-end
      (if (and (> (string-length line) 0)
               (char=? (string-ref line (sub1 (string-length line))) #\return))
          (sub1 (string-length line))
          (string-length line)))
    (define content (substring line 0 content-end))
    (define positions (regexp-match-positions #px"[[:space:]]+$" content))
    (when (and positions
               (diagnostic-for? diagnostics 'style/trailing-whitespace line-number))
      (define start (+ offset (caar positions)))
      (define end (+ offset (cdar positions)))
      (set! edits
            (cons (fix-edit start end "" 'style/trailing-whitespace
                            line-number (caar positions)
                            (substring text start end))
                  edits)))
    (set! offset (+ offset (string-length line) 1)))
  (when (and (not (string-suffix? text "\n"))
             (diagnostic-for? diagnostics 'style/newline-at-eof 1))
    (define last-line
      (if (null? lines) 1 (length lines)))
    (define last-col
      (if (null? lines) 0 (string-length (last lines))))
    (set! edits
          (cons (fix-edit (string-length text) (string-length text) "\n"
                          'style/newline-at-eof last-line last-col "")
                edits)))
  edits)

(define (parts-of stx)
  (and (syntax? stx) (syntax->list stx)))

(define (head-name parts)
  (and (pair? parts) (identifier? (first parts)) (syntax-e (first parts))))

(define (read-source-syntax path text)
  (with-handlers ([exn? (lambda (_) #f)])
    (define in (open-input-string text))
    (port-count-lines! in)
    (with-module-reading-parameterization
     (lambda () (read-syntax path in)))))

(define (cond-edits path text diagnostics)
  (define root (read-source-syntax path text))
  (define edits '())
  (define (walk stx)
    (define parts (parts-of stx))
    (unless (or (not parts)
                (memq (head-name parts) '(quote quasiquote syntax quasisyntax)))
      (when (eq? (head-name parts) 'cond)
        (define clauses (rest parts))
        (when (pair? clauses)
          (define last-clause (last clauses))
          (define clause-parts (parts-of last-clause))
          (when (and clause-parts (pair? clause-parts))
            (define test (first clause-parts))
            (when (and (eq? (syntax-e test) #t)
                       (diagnostic-for? diagnostics
                                        'style/simplify-cond
                                        (or (syntax-line test) 1)
                                        (or (syntax-column test) 0))
                       (syntax-position test)
                       (syntax-span test))
              (define start (sub1 (syntax-position test)))
              (define end (+ start (syntax-span test)))
              (when (and (<= 0 start) (<= end (string-length text))
                         (string=? (substring text start end) "#t"))
                (set! edits
                      (cons (fix-edit start end "else" 'style/simplify-cond
                                      (or (syntax-line test) 1)
                                      (or (syntax-column test) 0)
                                      "#t")
                            edits)))))))
      (for ([child (in-list (rest parts))])
        (walk child))))
  (when root (walk root))
  edits)

(define (replace-edits text edits)
  (define ordered
    (sort (remove-duplicates edits) > #:key fix-edit-start))
  (let loop ([remaining ordered] [result text] [last-start (string-length text)])
    (if (null? remaining)
        result
        (let ([edit (first remaining)])
          (if (and (<= (fix-edit-end edit) last-start)
                   (<= (fix-edit-start edit) (fix-edit-end edit))
                   (<= (fix-edit-end edit) (string-length result))
                   (string=? (substring result (fix-edit-start edit)
                                       (fix-edit-end edit))
                             (fix-edit-before edit)))
              (loop (rest remaining)
                    (string-append
                     (substring result 0 (fix-edit-start edit))
                     (fix-edit-replacement edit)
                     (substring result (fix-edit-end edit)))
                    (fix-edit-start edit))
              #f)))))

(define (find-edits path text diagnostics)
  (append (line-ending-edit text diagnostics)
          (cond
            [(ormap (lambda (finding)
                     (eq? (diagnostic-rule-id finding) 'style/simplify-cond))
                   diagnostics)
             (cond-edits path text diagnostics)]
            [else '()])))

(define (apply-safe-fixes path text diagnostics)
  (define applicable-diagnostics
    (filter (lambda (finding)
              (safe-fix-rule? (diagnostic-rule-id finding)))
            diagnostics))
  (cond
    [(null? applicable-diagnostics)
     (fix-result text #f #f "No safe fixer applies" '())]
    [else
     (define edits (find-edits path text applicable-diagnostics))
     (define once (replace-edits text edits))
     (cond
       [(not once)
        (fix-result text #f #f "A safe fix was no longer applicable" edits)]
       [else
        (define second-edits (find-edits path once applicable-diagnostics))
        (define twice (replace-edits once second-edits))
        (if (and twice (string=? once twice))
            (fix-result once (not (string=? text once)) #t "" edits)
            (fix-result text #f #f
                        "Safe fix rejected because it is not idempotent"
                        edits))])]))
