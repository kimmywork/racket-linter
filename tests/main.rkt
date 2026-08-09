#lang racket/base

(require rackunit
         "../core/diagnostic.rkt"
         "../core/rule.rkt"
         "../core/engine.rkt"
         "../rules/style/line-length.rkt"
         "../rules/style/trailing-whitespace.rkt"
         "../rules/style/newline-at-eof.rkt"
         "../rules/reachability/undefined.rkt"
         racket/port
         racket/file)

;; Test diagnostic creation
(test-case "diagnostic struct"
  (define d (diagnostic "test.rkt" 1 10 'warning 'test-rule "Test message"))
  (check-equal? (diagnostic-path d) "test.rkt")
  (check-equal? (diagnostic-line d) 1)
  (check-equal? (diagnostic-col d) 10)
  (check-equal? (diagnostic-severity d) 'warning)
  (check-equal? (diagnostic-rule-id d) 'test-rule)
  (check-equal? (diagnostic-message d) "Test message"))

;; Test rule struct
(test-case "rule struct"
  (define r (rule 'test-rule 'warning (hash 'enabled #t) (lambda (stx path config) '()) 'text))
  (check-equal? (rule-id r) 'test-rule)
  (check-equal? (rule-severity r) 'warning)
  (check-true (hash-ref (rule-config-keys r) 'enabled))
  (check-equal? (rule-layer r) 'text))

;; Test line-length rule
(test-case "line-length rule"
  (define long-line (make-string 110 #\a))
  (define short-line "short")
  (define temp-file (make-temporary-file "test-~a.rkt"))
  (call-with-output-file temp-file
    (lambda (out)
      (display long-line out)
      (newline out)
      (display short-line out))
    #:exists 'replace)
  (define config (hash))
  (define diags ((rule-check style/line-length) #f (path->string temp-file) config))
  (check-equal? (length diags) 1)
  (check-equal? (diagnostic-rule-id (car diags)) 'style/line-length)
  (delete-file temp-file))

;; Test trailing-whitespace rule
(test-case "trailing-whitespace rule"
  (define temp-file (make-temporary-file "test-~a.rkt"))
  (call-with-output-file temp-file
    (lambda (out)
      (display "line with trailing   " out)
      (newline out)
      (display "clean line" out)
      (newline out))
    #:exists 'replace)
  (define config (hash))
  (define diags ((rule-check style/trailing-whitespace) #f (path->string temp-file) config))
  (check-equal? (length diags) 1)
  (check-equal? (diagnostic-rule-id (car diags)) 'style/trailing-whitespace)
  (delete-file temp-file))

;; Test newline-at-eof rule
(test-case "newline-at-eof rule"
  (define temp-file (make-temporary-file "test-~a.rkt"))
  (call-with-output-file temp-file
    (lambda (out)
      (display "no newline at end"))
    #:exists 'replace)
  (define config (hash))
  (define diags ((rule-check style/newline-at-eof) #f (path->string temp-file) config))
  (check-equal? (length diags) 1)
  (check-equal? (diagnostic-rule-id (car diags)) 'style/newline-at-eof)
  (delete-file temp-file))

;; Test merge-configs
(test-case "merge-configs"
  (define default (hash 'a (hash 'x 1 'y 2) 'b (hash 'z 3)))
  (define user (hash 'a (hash 'x 10) 'c (hash 'w 4)))
  (define merged (merge-configs default user))
  (check-equal? (hash-ref (hash-ref merged 'a) 'x) 10)
  (check-equal? (hash-ref (hash-ref merged 'a) 'y) 2)
  (check-equal? (hash-ref (hash-ref merged 'b) 'z) 3)
  (check-equal? (hash-ref (hash-ref merged 'c) 'w) 4))
