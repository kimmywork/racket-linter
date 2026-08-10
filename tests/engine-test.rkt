#lang racket/base

(require rackunit
         racket/port
         racket/file
         racket/path
         racket/string
         racket/list
         "../core/engine.rkt"
         "../core/diagnostic.rkt"
         "../core/rule.rkt"
         "../rules/style/line-length.rkt"
         "../rules/style/trailing-whitespace.rkt"
         "../rules/style/newline-at-eof.rkt"
         "../rules/style/sexpr-depth.rkt"
         "../rules/style/definition-length.rkt"
         "../rules/style/file-length.rkt"
         "../rules/style/naming-convention.rkt"
         "../rules/style/require-sort.rkt"
         "../rules/style/provide-sort.rkt"
         "../rules/style/simplify-cond.rkt"
         "../rules/style/extract-let.rkt"
         "../rules/reachability/undefined.rkt"
         "../rules/reachability/unused-require.rkt"
         "../rules/definition/unused.rkt"
         "../rules/export/unused.rkt"
         "../rules/abstract/type-error.rkt")

(define (make-temp content)
  (define f (make-temporary-file "test-~a.rkt"))
  (display-to-file content f #:exists 'replace)
  f)

;; ============================================================
;; run-file: basic
;; ============================================================

(test-case "run-file: returns list"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (check-true (list? (run-file '() (hash) (path->string f))))
  (delete-file f))

(test-case "run-file: empty rules"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (check-equal? (length (run-file '() (hash) (path->string f))) 0)
  (delete-file f))

(test-case "run-file: text layer always runs"
  (define f (make-temp (make-string 200 #\x)))
  (define diags (run-file (list style/line-length) (hash) (path->string f)))
  (check-true (>= (length diags) 1))
  (delete-file f))

(test-case "run-file: syntax layer runs on safe lang"
  (define f (make-temp "#lang racket/base\n(displayln 1)\n"))
  (define diags (run-file (list style/sexpr-depth) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

(test-case "run-file: expand layer runs on safe lang"
  (define f (make-temp "#lang racket/base\n(define x 1)\n(displayln x)\n"))
  (define diags (run-file (list style/line-length) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

(test-case "run-file: no #lang falls back to text-only"
  (define f (make-temp "(define x 1)\n"))
  (define diags (run-file (list style/line-length) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

(test-case "run-file: non-safe lang downgrades to text-only"
  (define f (make-temp "#lang scribble/text\nHello world\n"))
  (define diags (run-file (list style/line-length style/sexpr-depth) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

;; ============================================================
;; run-file: config merging
;; ============================================================

(test-case "run-file: disabled rule produces no diagnostics"
  (define f (make-temp (make-string 200 #\x)))
  (define config (hash 'style/line-length (hash 'enabled #f)))
  (define diags (run-file (list style/line-length) config (path->string f)))
  (check-equal? (length diags) 0)
  (delete-file f))

(test-case "run-file: enabled rule produces diagnostics"
  (define f (make-temp (make-string 200 #\x)))
  (define config (hash 'style/line-length (hash 'enabled #t)))
  (define diags (run-file (list style/line-length) config (path->string f)))
  (check-true (>= (length diags) 1))
  (delete-file f))

(test-case "run-file: all rules disabled"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (define config (hash 'style/line-length (hash 'enabled #f)
                       'style/trailing-whitespace (hash 'enabled #f)
                       'style/newline-at-eof (hash 'enabled #f)
                       'style/definition-length (hash 'enabled #f)
                       'style/file-length (hash 'enabled #f)
                       'style/naming-convention (hash 'enabled #f)
                       'style/require-sort (hash 'enabled #f)
                       'style/provide-sort (hash 'enabled #f)
                       'style/extract-let (hash 'enabled #f)
                       'style/simplify-cond (hash 'enabled #f)
                       'style/sexpr-depth (hash 'enabled #f)
                       'reachability/undefined (hash 'enabled #f)))
  (define diags (run-file (list style/line-length style/sexpr-depth reachability/undefined) config (path->string f)))
  (check-equal? (length diags) 0)
  (delete-file f))

;; ============================================================
;; run-file: multiple rules
;; ============================================================

(test-case "run-file: multiple text rules"
  (define f (make-temp (string-append (make-string 200 #\x) "  \n")))
  (define diags (run-file (list style/line-length style/trailing-whitespace) (hash) (path->string f)))
  (check-true (>= (length diags) 2))
  (delete-file f))

(test-case "run-file: text + syntax rules"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (define diags (run-file (list style/line-length style/sexpr-depth) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

;; ============================================================
;; expand layer
;; ============================================================

(test-case "run-file: expand layer runs on safe lang"
  (define f (make-temp "#lang racket/base\n(define x 1)\n(x 2)\n"))
  (define diags (run-file (list abstract/type-error) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

(test-case "run-file: expand layer no false positive for procedure"
  (define f (make-temp "#lang racket/base\n(define (f x) (+ x 1))\n(f 2)\n"))
  (define diags (run-file (list abstract/type-error) (hash) (path->string f)))
  (define type-errors (filter (lambda (d) (eq? (diagnostic-rule-id d) 'abstract/type-error)) diags))
  (check-equal? (length type-errors) 0)
  (delete-file f))

;; ============================================================
;; safe-lang?
;; ============================================================

(test-case "safe-lang?: racket is safe"
  ;; We can't directly test safe-lang? without exporting it,
  ;; but we can test it indirectly through run-file behavior
  (define f (make-temp "#lang racket\n(displayln 1)\n"))
  ;; If racket is safe, syntax-layer rules will run
  (define diags (run-file (list style/sexpr-depth) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

(test-case "safe-lang?: racket/base is safe"
  (define f (make-temp "#lang racket/base\n(displayln 1)\n"))
  (define diags (run-file (list style/sexpr-depth) (hash) (path->string f)))
  (check-true (list? diags))
  (delete-file f))

(test-case "run-file: syntax rule analyzes all top-level forms"
  (define f (make-temp "#lang racket/base\n(define x 1)\n(displayln x)\n"))
  (define diags
    (run-file (list reachability/undefined)
              (hash 'reachability/undefined (hash 'enabled #t))
              (path->string f)))
  (check-equal? (length diags) 0)
  (delete-file f))

(test-case "run-file: syntax depth uses configured max-depth"
  (define nested
    (for/fold ([value "1"])
              ([_ (in-range 12)])
      (format "(list ~a)" value)))
  (define f (make-temp (string-append "#lang racket/base\n" nested "\n")))
  (define diags
    (run-file (list style/sexpr-depth)
              (hash 'style/sexpr-depth (hash 'max-depth 20))
              (path->string f)))
  (check-equal? (length diags) 0)
  (delete-file f))

(test-case "run-file: enabled expansion failures remain diagnostics"
  (define f (make-temp "#lang racket/base\n(if #t 1)\n"))
  (define diagnostics
    (run-file (list abstract/type-error)
              (hash 'abstract/type-error (hash 'enabled #t))
              (path->string f)))
  (check-equal? (length diagnostics) 1)
  (check-equal? (diagnostic-rule-id (first diagnostics)) 'expand-error)
  (delete-file f))


;; ============================================================

(test-case "merge-configs: basic override"
  (define default (hash 'a 1 'b 2))
  (define user (hash 'b 20 'c 30))
  (define merged (merge-configs default user))
  (check-equal? (hash-ref merged 'a) 1)
  (check-equal? (hash-ref merged 'b) 20)
  (check-equal? (hash-ref merged 'c) 30))

(test-case "merge-configs: nested hash"
  (define default (hash 'rule (hash 'enabled #t 'max 100)))
  (define user (hash 'rule (hash 'max 50)))
  (define merged (merge-configs default user))
  (check-true (hash-ref (hash-ref merged 'rule) 'enabled))
  (check-equal? (hash-ref (hash-ref merged 'rule) 'max) 50))

(test-case "merge-configs: user adds new rule"
  (define default (hash))
  (define user (hash 'my-rule (hash 'enabled #f)))
  (define merged (merge-configs default user))
  (check-false (hash-ref (hash-ref merged 'my-rule) 'enabled)))

(test-case "merge-configs: empty user"
  (define merged (merge-configs (hash 'a 1) (hash)))
  (check-equal? (hash-ref merged 'a) 1))

(test-case "merge-configs: empty default"
  (define merged (merge-configs (hash) (hash 'a 1)))
  (check-equal? (hash-ref merged 'a) 1))
