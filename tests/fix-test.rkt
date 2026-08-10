#lang racket/base

(require rackunit
         racket/list
         "../core/diagnostic.rkt"
         "../core/fix.rkt")

(test-case "trailing whitespace fix preserves indentation and is idempotent"
  (define text "  (define x 1)  \n\t(displayln x)\t\r\n")
  (define finding
    (diagnostic "sample.rkt" 1 0 'warning 'style/trailing-whitespace "trailing"))
  (define second-finding
    (diagnostic "sample.rkt" 2 0 'warning 'style/trailing-whitespace "trailing"))
  (define result
    (apply-safe-fixes "sample.rkt" text (list finding second-finding)))
  (check-true (fix-result-applicable? result))
  (check-true (fix-result-changed? result))
  (check-equal? (fix-result-text result) "  (define x 1)\n\t(displayln x)\r\n")
  (check-equal? (fix-result-text
                (apply-safe-fixes "sample.rkt"
                                  (fix-result-text result)
                                  (list finding second-finding)))
                (fix-result-text result)))

(test-case "EOF newline fix only appends a missing newline"
  (define finding
    (diagnostic "sample.rkt" 1 1 'warning 'style/newline-at-eof "missing"))
  (define result (apply-safe-fixes "sample.rkt" "(define x 1)" (list finding)))
  (check-equal? (fix-result-text result) "(define x 1)\n")
  (check-true (fix-result-changed? result))
  (define already-newline
    (apply-safe-fixes "sample.rkt" "(define x 1)\n" (list finding)))
  (check-false (fix-result-changed? already-newline))
  (check-equal? (fix-result-text already-newline) "(define x 1)\n"))

(test-case "unsupported transformations are never auto-applied"
  (define text "(provide z a)\n")
  (define finding
    (diagnostic "sample.rkt" 1 0 'warning 'style/provide-sort "sort"))
  (define result (apply-safe-fixes "sample.rkt" text (list finding)))
  (check-false (fix-result-applicable? result))
  (check-false (fix-result-changed? result))
  (check-equal? (fix-result-text result) text))

(test-case "final literal true cond fix uses an exact syntax span"
  (define text
    "#lang racket/base\n(cond [#f 'no]\n      [#t 'yes])\n")
  (define finding
    (diagnostic "sample.rkt" 3 7 'info 'style/simplify-cond
                "use else instead of #t for the final cond clause"))
  (define result (apply-safe-fixes "sample.rkt" text (list finding)))
  (check-true (fix-result-applicable? result))
  (check-true (fix-result-changed? result))
  (check-equal? (fix-result-text result)
                "#lang racket/base\n(cond [#f 'no]\n      [else 'yes])\n")
  (check-equal? (length (fix-result-edits result)) 1)
  (define edit (first (fix-result-edits result)))
  (check-equal? (fix-edit-before edit) "#t")
  (check-equal? (fix-edit-replacement edit) "else"))

(test-case "cond fix refuses a diagnostic that does not match the syntax span"
  (define text "#lang racket/base\n(cond [#t 'yes])\n")
  (define finding
    (diagnostic "sample.rkt" 2 99 'info 'style/simplify-cond "stale"))
  (define result (apply-safe-fixes "sample.rkt" text (list finding)))
  (check-false (fix-result-changed? result))
  (check-equal? (fix-result-text result) text))
