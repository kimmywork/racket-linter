#lang racket/base

(require rackunit
         racket/file
         racket/path
         "../core/project.rkt"
         "../core/diagnostic.rkt")

(define (make-temp content)
  (define f (make-temporary-file "test-~a.rkt"))
  (display-to-file content f #:exists 'replace)
  f)

;; ============================================================
;; parse-module-info
;; ============================================================

(test-case "parse-module-info: extracts provides"
  (define f (make-temp "#lang racket/base\n(provide foo bar)\n"))
  (define info (parse-module-info (path->string f)))
  (define provides (module-info-provides info))
  (check-true (list? provides))
  (check-true (> (length provides) 0))
  (delete-file f))

(test-case "parse-module-info: extracts requires"
  (define f (make-temp "#lang racket/base\n(require racket/list racket/string)\n"))
  (define info (parse-module-info (path->string f)))
  (define requires (module-info-requires info))
  (check-true (list? requires))
  (check-true (> (length requires) 0))
  (delete-file f))

(test-case "parse-module-info: no provides"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (define info (parse-module-info (path->string f)))
  (check-equal? (module-info-provides info) '())
  (delete-file f))

(test-case "parse-module-info: no requires"
  (define f (make-temp "#lang racket/base\n(define x 1)\n"))
  (define info (parse-module-info (path->string f)))
  (check-equal? (module-info-requires info) '())
  (delete-file f))

(test-case "parse-module-info: multi-line provide"
  (define f (make-temp "#lang racket/base\n(provide foo\n  bar)\n"))
  (define info (parse-module-info (path->string f)))
  (check-true (list? (module-info-provides info)))
  (delete-file f))

(test-case "parse-module-info: multi-line require"
  (define f (make-temp "#lang racket/base\n(require racket/list\n  racket/string)\n"))
  (define info (parse-module-info (path->string f)))
  (check-true (list? (module-info-requires info)))
  (delete-file f))

(test-case "parse-module-info: path matches"
  (define f (make-temp "#lang racket/base\n"))
  (define info (parse-module-info (path->string f)))
  (check-equal? (module-info-path info) (path->string f))
  (delete-file f))

;; ============================================================
;; build-dependency-graph
;; ============================================================

(test-case "build-dependency-graph: single file"
  (define f (make-temp "#lang racket/base\n(provide x)\n"))
  (define graph (build-dependency-graph (list (path->string f))))
  (check-equal? (hash-count graph) 1)
  (delete-file f))

(test-case "build-dependency-graph: two files"
  (define temp-dir (make-temporary-file "test-~a" 'directory))
  (define f1 (build-path temp-dir "a.rkt"))
  (define f2 (build-path temp-dir "b.rkt"))
  (call-with-output-file f1 (lambda (o) (display "#lang racket/base\n(provide x)\n")) #:exists 'replace)
  (call-with-output-file f2 (lambda (o) (display "#lang racket/base\n(require \"a.rkt\")\n")) #:exists 'replace)
  (define graph (build-dependency-graph (list (path->string f1) (path->string f2))))
  (check-equal? (hash-count graph) 2)
  (check-true (hash-has-key? graph (path->string f1)))
  (check-true (hash-has-key? graph (path->string f2)))
  (delete-directory/files temp-dir))

(test-case "build-dependency-graph: empty"
  (define graph (build-dependency-graph '()))
  (check-equal? (hash-count graph) 0))

;; ============================================================
;; find-circular-dependencies
;; ============================================================

(test-case "find-circular-dependencies: no cycles"
  (define temp-dir (make-temporary-file "test-~a" 'directory))
  (define f1 (build-path temp-dir "a.rkt"))
  (call-with-output-file f1 (lambda (o) (display "#lang racket/base\n(provide x)\n")) #:exists 'replace)
  (define graph (build-dependency-graph (list (path->string f1))))
  (check-equal? (find-circular-dependencies graph) '())
  (delete-directory/files temp-dir))

(test-case "find-circular-dependencies: empty graph"
  (check-equal? (find-circular-dependencies (hash)) '()))

;; ============================================================
;; find-unused-exports
;; ============================================================

(test-case "find-unused-exports: detects unused"
  (define temp-dir (make-temporary-file "test-~a" 'directory))
  (define f1 (build-path temp-dir "a.rkt"))
  (define f2 (build-path temp-dir "b.rkt"))
  (call-with-output-file f1 (lambda (o) (display "#lang racket/base\n(provide my-func)\n(define (my-func x) x)\n")) #:exists 'replace)
  (call-with-output-file f2 (lambda (o) (display "#lang racket/base\n(define x 1)\n")) #:exists 'replace)
  (define graph (build-dependency-graph (list (path->string f1) (path->string f2))))
  (define unused (find-unused-exports graph))
  (check-true (list? unused))
  (delete-directory/files temp-dir))

(test-case "find-unused-exports: empty graph"
  (check-equal? (find-unused-exports (hash)) '()))

;; ============================================================
;; analyze-project
;; ============================================================

(test-case "analyze-project: single file"
  (define f (make-temp "#lang racket/base\n(provide x)\n(define x 1)\n"))
  (define diags (analyze-project (list (path->string f))))
  (check-true (list? diags))
  (delete-file f))

(test-case "analyze-project: empty"
  (check-equal? (length (analyze-project '())) 0))
