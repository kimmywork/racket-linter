#lang racket/base

(require rackunit
         "../core/diagnostic.rkt"
         "../core/rule.rkt"
         "../core/engine.rkt")

;; diagnostic struct
(test-case "diagnostic struct fields"
  (define d (diagnostic "test.rkt" 10 5 'warning 'my-rule "msg"))
  (check-equal? (diagnostic-path d) "test.rkt")
  (check-equal? (diagnostic-line d) 10)
  (check-equal? (diagnostic-col d) 5)
  (check-equal? (diagnostic-severity d) 'warning)
  (check-equal? (diagnostic-rule-id d) 'my-rule)
  (check-equal? (diagnostic-message d) "msg"))

(test-case "diagnostic transparency"
  (define d (diagnostic "a.rkt" 1 1 'error 'r "m"))
  (check-true (diagnostic? d))
  (check-equal? d (diagnostic "a.rkt" 1 1 'error 'r "m")))

;; rule struct
(test-case "rule struct fields"
  (define r (rule 'test 'info (hash 'enabled #t) (lambda (s p c) '()) 'text))
  (check-equal? (rule-id r) 'test)
  (check-equal? (rule-severity r) 'info)
  (check-true (hash-ref (rule-config-keys r) 'enabled))
  (check-equal? (rule-layer r) 'text)
  (check-true (procedure? (rule-check r))))

;; merge-configs
(test-case "merge-configs basic"
  (define default (hash 'a 1 'b 2))
  (define user (hash 'b 20 'c 30))
  (define merged (merge-configs default user))
  (check-equal? (hash-ref merged 'a) 1)
  (check-equal? (hash-ref merged 'b) 20)
  (check-equal? (hash-ref merged 'c) 30))

(test-case "merge-configs nested"
  (define default (hash 'rule1 (hash 'enabled #t 'max 100)))
  (define user (hash 'rule1 (hash 'max 50)))
  (define merged (merge-configs default user))
  (check-true (hash-ref (hash-ref merged 'rule1) 'enabled))
  (check-equal? (hash-ref (hash-ref merged 'rule1) 'max) 50))

(test-case "merge-configs user adds new keys"
  (define default (hash))
  (define user (hash 'rule1 (hash 'enabled #f)))
  (define merged (merge-configs default user))
  (check-false (hash-ref (hash-ref merged 'rule1) 'enabled)))

(test-case "merge-configs empty user"
  (define default (hash 'a 1))
  (define merged (merge-configs default (hash)))
  (check-equal? (hash-ref merged 'a) 1))
