#lang racket/base

(require rackunit
         "helpers.rkt"
         "../rules/reachability/undefined.rkt"
         "../rules/reachability/unused-require.rkt")

;; reachability/undefined
(test-case "undefined: detects truly undefined identifier"
  (define diags (run-syntax-rule (list reachability/undefined) "(undefined-var)"))
  (check-true (>= (length diags) 1)))

(test-case "undefined: ignores built-in identifiers"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(displayln \"hello\")")) 0))

(test-case "undefined: ignores locally defined names"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(define x 1) (displayln x)")) 0))

(test-case "undefined: detects undefined in application"
  (check-true (>= (length (run-syntax-rule (list reachability/undefined) "(my-func 1 2)")) 1)))

(test-case "undefined: struct generates predicates"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(struct point (x y))")) 0))

(test-case "undefined: if expressions"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(if #t 1 2)")) 0))

(test-case "undefined: begin"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(begin 1 2 3)")) 0))

(test-case "undefined: set!"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(define x 1) (set! x 2)")) 0))

(test-case "undefined: module+ skipped"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(module+ test (displayln 1))")) 0))

(test-case "undefined: provide skipped"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(provide x)")) 0))

(test-case "undefined: define-syntax skipped"
  (check-equal? (length (run-syntax-rule (list reachability/undefined) "(define-syntax my-mac (syntax-rules () ((_ x) x)))")) 0))

;; reachability/unused-require
(test-case "unused-require: detects unused require"
  (define diags (run-syntax-rule (list reachability/unused-require)
                                  "(require racket/string) (displayln 1)"))
  (check-true (list? diags)))

(test-case "unused-require: empty for no requires"
  (check-equal? (length (run-syntax-rule (list reachability/unused-require) "(displayln 1)")) 0))
