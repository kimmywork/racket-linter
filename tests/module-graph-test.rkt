#lang racket/base

(require rackunit
         racket/file
         racket/path
         racket/list
         "../core/module-graph.rkt"
         "../core/diagnostic.rkt")

(define (write-module dir name content)
  (define path (build-path dir name))
  (call-with-output-file path (lambda (out) (display content out)) #:exists 'replace)
  path)

(test-case "module facts preserve definitions, provides, and phases"
  (define dir (make-temporary-file "module-graph-~a" 'directory))
  (define path
    (write-module dir "a.rkt"
                  "#lang racket/base\n(provide x)\n(require (for-syntax racket/list) (only-in racket/string string-append))\n(define x 1)\n"))
  (define facts (parse-module-facts (path->string path)))
  (check-not-false (member 'x (module-facts-definitions facts)))
  (check-not-false (member 'x (module-facts-provides facts)))
  (check-equal? (length (module-facts-requires facts)) 2)
  (check-not-false (member 'for-syntax
                          (map require-edge-phase (module-facts-requires facts))))
  (check-not-false (member 'normal
                          (map require-edge-phase (module-facts-requires facts))))
  (delete-directory/files dir))

(test-case "module graph resolves string relative requires"
  (define dir (make-temporary-file "module-graph-~a" 'directory))
  (define a (write-module dir "a.rkt" "#lang racket/base\n(provide x)\n(define x 1)\n"))
  (define b (write-module dir "b.rkt" "#lang racket/base\n(require \"a.rkt\")\n(displayln 1)\n"))
  (define graph (build-phase-module-graph (list (path->string a) (path->string b))))
  (check-equal? (length (module-facts-requires (hash-ref graph (path->string b)))) 1)
  (check-equal? (length (check-phase-module-graph (list (path->string a) (path->string b)))) 0)
  (delete-directory/files dir))

(test-case "module graph reports unresolved relative requires"
  (define dir (make-temporary-file "module-graph-~a" 'directory))
  (define a (write-module dir "a.rkt" "#lang racket/base\n(require \"missing.rkt\")\n"))
  (define diagnostics (check-phase-module-graph (list (path->string a))))
  (check-equal? (length diagnostics) 1)
  (check-equal? (diagnostic-rule-id (first diagnostics))
                'module/phase-unresolved-require)
  (delete-directory/files dir))

(test-case "module graph detects phase-aware cycles"
  (define dir (make-temporary-file "module-graph-~a" 'directory))
  (define a (write-module dir "a.rkt" "#lang racket/base\n(require (for-syntax \"b.rkt\"))\n"))
  (define b (write-module dir "b.rkt" "#lang racket/base\n(require (for-syntax \"a.rkt\"))\n"))
  (define diagnostics
    (check-phase-module-graph (list (path->string a) (path->string b))))
  (check-true (ormap (lambda (d) (eq? (diagnostic-rule-id d) 'module/phase-cycle))
                     diagnostics))
  (delete-directory/files dir))

(test-case "module graph skips collection requires"
  (define dir (make-temporary-file "module-graph-~a" 'directory))
  (define a (write-module dir "a.rkt" "#lang racket/base\n(require racket/list)\n"))
  (check-equal? (length (check-phase-module-graph (list (path->string a)))) 0)
  (delete-directory/files dir))
