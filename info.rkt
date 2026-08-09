#lang info
(define collection "racket-linter")
(define version "0.2.0")
(define deps '("base" "drracket-tool-text-lib"))
(define build-deps '("rackunit-lib" "scribble-lib" "cover"))
(define test-omit-paths '("tests"))
(define scribblings '(("scribblings/racket-linter.scrbl" ())))
(define pkg-desc "A configurable, extensible Racket linter with style, reachability, and abstract interpretation rules")
(define pkg-authors '(kimmy))
(define pkg-license 'MIT)
(define raco-commands
  '(("lint" racket-linter/cli/lint "Lint Racket files" 1)))
