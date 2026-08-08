#lang info
(define collection "racket-linter")
(define version "0.1")
(define deps '("base"))
(define build-deps '("rackunit-lib"))
(define test-omit-paths '("tests"))
(define pkg-desc "A configurable, extensible Racket linter")
(define pkg-authors '(kimmy))
(define raco-commands
  '(("lint" "cli/lint" "Lint Racket files" 1)))
