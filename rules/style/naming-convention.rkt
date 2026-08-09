#lang racket/base

(require
  racket/port
  racket/contract/base
  racket/syntax
  racket/list
  racket/string
  racket/set
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide style/naming-convention)

;; Racket naming conventions:
;; - Use hyphens, not underscores or camelCase
;; - Predicates end with ?
;; - Mutators end with !

(define keywords
  (list "define" "lambda" "let" "letrec"
        "if" "cond" "case" "match" "and" "or"
        "begin" "set!" "quote" "require"
        "provide" "struct" "module" "module+"
        "define-values" "define-syntax"
        "define-syntaxes" "for-syntax"
        "for-template" "only-in" "except-in"
        "prefix-in" "rename-in" "all-defined-out"
        "all-from-out" "rename-out"
        "struct-out" "define-struct"))

(define (check-identifier id path ln)
  (if (and (> (string-length id) 1)
           (not (member id keywords)))
      (cond
        ((string-contains? id "_")
         (diagnostic path ln 1 'info 'style/naming-convention
                     (format "Identifier ~a uses underscore; prefer hyphens" id)))
        ((and (regexp-match? #px"[a-z][A-Z]" id)
              (not (string=? id (string-downcase id))))
         (diagnostic path ln 1 'info 'style/naming-convention
                     (format "Identifier ~a uses camelCase; prefer hyphens" id)))
        (else #f))
      #f))

(define-rule style/naming-convention
  #:id 'style/naming-convention
  #:severity 'info
  #:config-keys (hash 'enabled #f)
  #:layer 'text
  (lambda (stx path config)
    (define text (port->string (open-input-file path)))
    (define lines (string-split text "\n"))
    
    (for/fold ([acc '()]) ([line (in-list lines)] [ln (in-naturals 1)])
      (define trimmed (string-trim line))
      (if (or (string=? "" trimmed) (string-prefix? trimmed ";"))
          acc
          (let* ([identifiers (regexp-match* #px"[a-zA-Z_][a-zA-Z0-9_?!%<>*/+-]*" trimmed)]
                 [results (map (lambda (id) (check-identifier id path ln)) identifiers)]
                 [filtered (filter (lambda (x) x) results)])
            (append acc filtered))))))
