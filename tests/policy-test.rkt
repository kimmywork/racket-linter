#lang racket/base

(require json
         rackunit
         racket/file
         racket/list
         racket/path
         racket/set
         "../core/baseline.rkt"
         "../core/diagnostic.rkt"
         "../core/suppression.rkt")

(define known-rules
  (set 'style/line-length 'style/trailing-whitespace))

(define (with-temp-file content proc)
  (define path (make-temporary-file "policy-~a.rkt"))
  (dynamic-wind
   void
   (lambda ()
     (display-to-file content path #:exists 'replace)
     (proc path))
   (lambda () (when (file-exists? path) (delete-file path)))))

(test-case "source suppression directives cover exact line and ranges"
  (with-temp-file
   (string-append
    "; racket-linter-disable-next-line style/line-length\n"
    "next\n"
    "; racket-linter-disable style/trailing-whitespace\n"
    "active\n"
    "; racket-linter-enable style/trailing-whitespace\n"
    "inactive\n"
    "; racket-linter-disable-line style/line-length\n")
   (lambda (path)
     (define-values (index errors)
       (read-suppressions (path->string path) known-rules))
     (check-equal? errors '())
     (check-true
      (diagnostic-suppressed?
       index (diagnostic path 2 0 'warning 'style/line-length "next")))
     (check-true
      (diagnostic-suppressed?
       index (diagnostic path 4 0 'warning 'style/trailing-whitespace "active")))
     (check-false
      (diagnostic-suppressed?
       index (diagnostic path 6 0 'warning 'style/trailing-whitespace "inactive")))
     (check-true
      (diagnostic-suppressed?
       index (diagnostic path 7 0 'warning 'style/line-length "same"))))))

(test-case "source suppression rejects unknown and malformed directives"
  (with-temp-file
   (string-append
    "; racket-linter-disable-next-line unknown/rule\n"
    "; racket-linter-disable-line\n"
    "; racket-linter-ignore style/line-length\n"
    "; racket-linter-enable style/line-length\n")
   (lambda (path)
     (define-values (_index errors)
       (read-suppressions (path->string path) known-rules))
     (check-equal?
      (map diagnostic-rule-id errors)
      '(suppression/unknown-rule
        suppression/invalid-directive
        suppression/invalid-directive
        suppression/unmatched-enable)))))

(test-case "baseline roundtrip filters exact diagnostics and reports stale entries"
  (define directory (make-temporary-file "baseline-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define source (build-path directory "sample.rkt"))
     (define baseline (build-path directory "baseline.json"))
     (display-to-file "#lang racket/base\n" source)
     (define original
       (diagnostic (path->string source) 3 2 'warning 'style/line-length "too long"))
     (write-baseline! baseline directory (list original))
     (define-values (entries errors) (read-baseline baseline known-rules))
     (check-equal? errors '())
     (check-equal? (length entries) 1)
     (check-equal? (baseline-entry-path (first entries)) "sample.rkt")
     (define-values (remaining stale)
       (apply-baseline entries baseline directory (list original)))
     (check-equal? remaining '())
     (check-equal? stale '())
     (define changed
       (diagnostic (path->string source) 3 2 'warning 'style/line-length "changed"))
     (define-values (changed-remaining changed-stale)
       (apply-baseline entries baseline directory (list changed)))
     (check-equal? changed-remaining (list changed))
     (check-equal? (map diagnostic-rule-id changed-stale)
                   '(baseline/stale-entry)))
   (lambda () (delete-directory/files directory))))

(test-case "baseline parser fails closed on unknown rule IDs"
  (define path (make-temporary-file "baseline-~a.json"))
  (dynamic-wind
   void
   (lambda ()
     (call-with-output-file path
       (lambda (out)
         (write-json
          (hash 'version 1
                'diagnostics
                (list (hash 'path "sample.rkt"
                            'line 1
                            'col 0
                            'rule-id "unknown/rule"
                            'message-hash "0000000000000000000000000000000000000000")))
          out))
       #:exists 'replace)
     (define-values (entries errors) (read-baseline path known-rules))
     (check-equal? entries '())
     (check-equal? (map diagnostic-rule-id errors) '(baseline/invalid-file)))
   (lambda () (delete-file path))))
