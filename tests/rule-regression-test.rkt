#lang racket/base

(require rackunit
         racket/list
         racket/string
         "helpers.rkt"
         "../core/engine.rkt"
         "../core/diagnostic.rkt"
         "../core/rule.rkt"
         "../rules/export/unused.rkt"
         "../rules/reachability/unused-require.rkt"
         "../rules/module/require-provide.rkt"
         "../rules/abstract/unreachable-code.rkt")

(define (run-enabled rule content)
  (define f (make-temp-rkt content))
  (define diagnostics
    (run-file (list rule)
              (hash (rule-id rule) (hash 'enabled #t))
              (path->string f)))
  (delete-file f)
  diagnostics)

(test-case "export/unused accepts an exported binding used in the module"
  (check-equal?
   (length
    (run-enabled export/unused
                  "#lang racket/base\n(provide used)\n(define used 1)\n(displayln used)\n"))
   0))

(test-case "export/unused reports an exported binding with no use"
  (define diagnostics
    (run-enabled export/unused
                 "#lang racket/base\n(provide unused)\n(define unused 1)\n"))
  (check-equal? (length diagnostics) 1)
  (check-equal? (diagnostic-rule-id (first diagnostics)) 'export/unused))

(test-case "unused-require recognizes only-in bindings and their uses"
  (define diagnostics
    (run-enabled reachability/unused-require
                 "#lang racket/base\n(require (only-in racket/list first second))\n(first '(1))\n"))
  (check-equal? (length diagnostics) 1)
  (check-true (string-contains?
               (diagnostic-message (first diagnostics))
               "second")))

(test-case "module/require-provide accepts local definitions"
  (check-equal?
   (length
    (run-enabled module/require-provide
                  "#lang racket/base\n(provide value)\n(define value 1)\n"))
   0))
(test-case "module/require-provide reports missing definitions"
  (define diagnostics
    (run-enabled module/require-provide
                 "#lang racket/base\n(provide missing)\n"))
  (check-equal? (length diagnostics) 1)
  (check-equal? (diagnostic-severity (first diagnostics)) 'warning))

(test-case "abstract/unreachable-code reports code after raise"
  (define diagnostics
    (run-enabled abstract/unreachable-code
                 "#lang racket/base\n(raise \"stop\")\n(displayln 1)\n"))
  (check-equal? (length diagnostics) 1)
  (check-equal? (diagnostic-rule-id (first diagnostics))
                'abstract/unreachable-code))

(test-case "abstract/unreachable-code accepts ordinary sequencing"
  (check-equal?
   (length
    (run-enabled abstract/unreachable-code
                 "#lang racket/base\n(displayln 1)\n(displayln 2)\n"))
   0))
