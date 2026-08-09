#lang racket/base

(require rackunit
         racket/string
         "helpers.rkt"
         "../core/diagnostic.rkt"
         "../core/rule.rkt"
         "../rules/style/line-length.rkt"
         "../rules/style/trailing-whitespace.rkt"
         "../rules/style/newline-at-eof.rkt"
         "../rules/style/naming-convention.rkt"
         "../rules/style/require-sort.rkt"
         "../rules/style/provide-sort.rkt"
         "../rules/style/simplify-cond.rkt"
         "../rules/style/definition-length.rkt"
         "../rules/style/file-length.rkt"
         "../rules/style/extract-let.rkt")

;; line-length
(test-case "line-length: detects long lines"
  (define diags (run-rule-on style/line-length
                              (string-append (make-string 150 #\x) "\nshort\n")))
  (check-equal? (length diags) 1)
  (check-equal? (diagnostic-line (car diags)) 1))

(test-case "line-length: ignores short lines"
  (check-equal? (length (run-rule-on style/line-length "short\n")) 0))

(test-case "line-length: multiple long lines"
  (define content (string-append (make-string 150 #\a) "\n" (make-string 200 #\b) "\n"))
  (check-equal? (length (run-rule-on style/line-length content)) 2))

(test-case "line-length: exact boundary"
  (check-equal? (length (run-rule-on style/line-length (string-append (make-string 102 #\a) "\n"))) 0)
  (check-equal? (length (run-rule-on style/line-length (string-append (make-string 103 #\a) "\n"))) 1))


(test-case "line-length: respects configured max-length"
  (define temp-file (make-temp-rkt (string-append (make-string 120 #\a) "\n")))
  (define diags
    ((rule-check style/line-length)
     #f
     (path->string temp-file)
     (hash 'max-length 120)))
  (delete-file temp-file)
  (check-equal? (length diags) 0))

(test-case "trailing-whitespace: detects trailing spaces"
  (check-equal? (length (run-rule-on style/trailing-whitespace "line with trailing   \n")) 1))

(test-case "trailing-whitespace: ignores clean lines"
  (check-equal? (length (run-rule-on style/trailing-whitespace "clean line\n")) 0))

(test-case "trailing-whitespace: detects trailing tabs"
  (check-equal? (length (run-rule-on style/trailing-whitespace "line\t\n")) 1))

(test-case "trailing-whitespace: multiple dirty lines"
  (check-equal? (length (run-rule-on style/trailing-whitespace "a  \nb  \n")) 2))

;; newline-at-eof
(test-case "newline-at-eof: detects missing newline"
  (check-equal? (length (run-rule-on style/newline-at-eof "no newline")) 1))

(test-case "newline-at-eof: ignores file with newline"
  (check-equal? (length (run-rule-on style/newline-at-eof "has newline\n")) 0))

(test-case "newline-at-eof: empty file"
  (check-equal? (length (run-rule-on style/newline-at-eof "")) 1))

;; naming-convention
(test-case "naming-convention: detects underscore"
  (define diags (run-rule-on style/naming-convention "my_variable\n"))
  (check-equal? (length diags) 1)
  (check-true (string-contains? (diagnostic-message (car diags)) "underscore")))

(test-case "naming-convention: detects camelCase"
  (define diags (run-rule-on style/naming-convention "myFunction\n"))
  (check-equal? (length diags) 1)
  (check-true (string-contains? (diagnostic-message (car diags)) "camelCase")))

(test-case "naming-convention: accepts hyphenated names"
  (check-equal? (length (run-rule-on style/naming-convention "my-good-var\n")) 0))

(test-case "naming-convention: skips single-char names"
  (check-equal? (length (run-rule-on style/naming-convention "x\n")) 0))

(test-case "naming-convention: skips keywords"
  (check-equal? (length (run-rule-on style/naming-convention "define\n")) 0))

(test-case "naming-convention: multiple violations"
  (check-equal? (length (run-rule-on style/naming-convention "my_var\nmyFunc\n")) 2))

;; require-sort
(test-case "require-sort: detects unsorted"
  (check-equal? (length (run-rule-on style/require-sort "(require racket/string racket/list)\n")) 1))

(test-case "require-sort: accepts sorted"
  (check-equal? (length (run-rule-on style/require-sort "(require racket/list racket/string)\n")) 0))

(test-case "require-sort: non-require line"
  (check-equal? (length (run-rule-on style/require-sort "(define x 1)\n")) 0))

;; provide-sort
(test-case "provide-sort: detects unsorted"
  (check-equal? (length (run-rule-on style/provide-sort "(provide zebra apple)\n")) 1))

(test-case "provide-sort: accepts sorted"
  (check-equal? (length (run-rule-on style/provide-sort "(provide apple zebra)\n")) 0))

(test-case "provide-sort: non-provide line"
  (check-equal? (length (run-rule-on style/provide-sort "(define x 1)\n")) 0))

;; simplify-cond
(test-case "simplify-cond: detects #t instead of else"
  (check-equal? (length (run-rule-on style/simplify-cond "  [#t (displayln x)]\n")) 1))

(test-case "simplify-cond: accepts else"
  (check-equal? (length (run-rule-on style/simplify-cond "  [else (displayln x)]\n")) 0))

(test-case "simplify-cond: cond with #t on same line"
  (check-true (>= (length (run-rule-on style/simplify-cond "(cond [#t 1])\n")) 1)))

;; definition-length
(test-case "definition-length: detects long definitions"
  (define lines (append (list "#lang racket/base\n" "(define (f)\n")
                        (for/list ([i 70]) (format "  (displayln ~a)\n" i))
                        (list ")\n")))
  (check-true (list? (run-rule-on style/definition-length (apply string-append lines)))))

(test-case "definition-length: short definition"
  (check-equal? (length (run-rule-on style/definition-length "#lang racket/base\n(define x 1)\n")) 0))

;; file-length
(test-case "file-length: detects long files"
  (define lines (for/list ([i 1100]) (format "line ~a\n" i)))
  (check-equal? (length (run-rule-on style/file-length (apply string-append lines))) 1))

(test-case "file-length: accepts short files"
  (define lines (for/list ([i 100]) (format "line ~a\n" i)))
  (check-equal? (length (run-rule-on style/file-length (apply string-append lines))) 0))

(test-case "file-length: exact boundary"
  (check-equal? (length (run-rule-on style/file-length (apply string-append (for/list ([i 999]) (format "l\n"))))) 0)
  (check-equal? (length (run-rule-on style/file-length (apply string-append (for/list ([i 1001]) (format "l\n"))))) 1))

;; extract-let
(test-case "extract-let: detects repeated expressions"
  (define diags (run-rule-on style/extract-let "(+ x y) (+ x y) (+ x y)\n"))
  (check-true (>= (length diags) 1)))

(test-case "extract-let: ignores unique expressions"
  (check-equal? (length (run-rule-on style/extract-let "(+ a b) (- c d) (* e f)\n")) 0))

(test-case "extract-let: ignores short expressions"
  (check-equal? (length (run-rule-on style/extract-let "(x) (x) (x)\n")) 0))
