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

(provide reachability/undefined)

(define-rule reachability/undefined
  #:id 'reachability/undefined
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    ;; Core forms and common built-in identifiers
    (define built-ins
      (set 'define 'define-values 'define-syntax 'define-syntaxes
           'lambda 'let 'let* 'letrec 'if 'cond 'case 'match
           'match-define 'match-let 'match-let*
           'and 'or 'not 'begin 'begin0 'set! 'quote 'quasiquote
           'unquote 'unquote-splicing 'syntax 'quasisyntax
           'with-syntax 'syntax-case 'syntax-rules
           'display 'displayln 'printf 'newline 'print 'println 'eprintf
           'list 'cons 'car 'cdr 'cadr 'caddr 'cddr 'cdddr
           'first 'second 'third 'fourth 'fifth 'rest 'last 'append 'reverse
           'null? 'pair? 'list? 'empty? 'not-pair?
           'eq? 'equal? 'eqv? '=? 'zero? 'positive? 'negative? 'even? 'odd?
           'number? 'string? 'symbol? 'boolean? 'procedure? 'void? 'eof-object?
           '+ '- '* '/ '= '< '> '<= '>= 'remainder 'modulo 'add1 'sub1 'expt 'abs
           'min 'max 'gcd 'lcm 'round 'floor 'ceiling 'truncate
           'number->string 'string->number 'symbol->string 'string->symbol
           'string-append 'string-length 'string-ref 'substring 'string-trim
           'string=? 'string<? 'string>? 'string<=? 'string>=?
           'string-upcase 'string-downcase 'string-contains? 'string-split
           'format 'fprintf 'with-output-to-string
           'apply 'map 'filter 'foldl 'foldr 'for-each 'andmap 'ormap
           'for/list 'for/fold 'for/sum 'for/product 'for/and 'for/or
           'for/first 'for/last 'for*/list 'for*/fold
           'in-range 'in-list 'in-naturals 'in-string 'in-vector 'in-hash
           'hash 'hash-set 'hash-ref 'hash-remove 'hash-has-key?
           'hash-count 'hash-empty? 'hash-keys 'hash-values 'hash->list
           'hash-map 'hash-for-each 'hash-union
           'make-hash 'make-hasheq 'make-hasheqv
           'vector 'vector-ref 'vector-set! 'vector-length 'vector->list 'list->vector
           'make-vector 'vector-map 'vector-for-each
           'error 'raise 'raise-argument-error 'raise-arguments-error
           'with-handlers 'exn? 'exn-message 'exn-continuation-marks
           'exn:fail? 'exn:fail:contract?
           'current-continuation-marks 'continuation-mark->first
           'call-with-current-continuation 'call/cc 'call-with-values
           'dynamic-wind 'parameterize 'make-parameter
           'require 'provide 'module 'module+
           'only-in 'except-in 'prefix-in 'rename-in 'file
           'for-syntax 'for-template 'for-label 'for-meta 'for-space
           'all-defined-out 'all-from-out 'rename-out 'except-out 'prefix-out
           'struct 'struct-out 'define-struct
           'submod 'begin-for-syntax 'begin-for-template
           'when 'unless 'cond 'case 'match 'match-define
           'open-input-string 'open-input-file 'open-output-string 'get-output-string
           'close-input-port 'close-output-port 'port->string 'file->string
           'call-with-input-file 'call-with-output-file 'with-input-from-file
           'current-input-port 'current-output-port 'current-error-port
           'read 'read-line 'read-string 'write 'display 'newline 'flush-output
           'path->string 'string->path 'path->complete-path
           'file-exists? 'directory-exists? 'build-path 'resolve-path
           'find-system-path 'current-directory
           'regexp-match 'regexp-match-positions 'regexp-replace 'regexp-replace*
           'regexp-split 'regexp-match? 'pregexp 'regexp
           'thread 'thread-wait 'thread-send 'thread-receive 'kill-thread
           'sync 'sync/timeout 'handle-evt 'choice-evt 'channel-get 'channel-put
           'make-channel 'place 'place-wait 'place-channel-get 'place-channel-put
           'random 'random-seed 'current-milliseconds 'current-inexact-milliseconds
           'sleep 'date->string 'current-date 'current-seconds
           '#%app '#%datum '#%top '#%top-interaction '#%module-begin
           '#%require '#%provide))

    (define (is-built-in? name)
      (set-member? built-ins name))

    ;; Parse require form to extract imported names
    (define (parse-require-form stx)
      (define e (syntax-e stx))
      (if (not (and (pair? e) (eq? (syntax-e (car e)) 'require)))
          '()
          (let ([rest (cdr e)])
            (for/fold ([acc '()]) ([req (in-list rest)])
              (let ([req-e (syntax-e req)])
                (cond
                  ;; Simple module path: (require racket/list)
                  ((symbol? req-e) acc)
                  ;; (only-in mod name ...)
                  ((and (pair? req-e) (eq? (car req-e) 'only-in))
                   (append acc (map (lambda (n) (syntax-e n)) (cddr req-e))))
                  ;; (except-in mod name ...) - can't determine remaining names
                  ((and (pair? req-e) (eq? (car req-e) 'except-in)) acc)
                  ;; (prefix-in prefix mod)
                  ((and (pair? req-e) (eq? (car req-e) 'prefix-in)) acc)
                  ;; (rename-in mod [old new] ...)
                  ((and (pair? req-e) (eq? (car req-e) 'rename-in))
                   (append acc (map (lambda (m) (syntax-e (cadr m))) (cddr req-e))))
                  ;; (file path)
                  ((and (pair? req-e) (eq? (car req-e) 'file)) acc)
                  ;; Unknown form
                  (else acc)))))))

    ;; Collect definitions and references from syntax
    (define (collect-info stx)
      (if (not (syntax? stx))
          '()
          (let ([e (syntax-e stx)])
            (cond
              ;; Definition form
              ((and (pair? e)
                    (let ([head (syntax-e (car e))])
                      (memq head '(define define-values define-syntax define-syntaxes))))
               (let ([head (syntax-e (car e))])
                 (cond
                   ((eq? head 'define)
                    (let* ([second-e (cadr e)]
                           [name (cond
                                   ((symbol? (syntax-e second-e)) (syntax-e second-e))
                                   ((pair? (syntax-e second-e)) (syntax-e (car (syntax-e second-e))))
                                   (else #f))])
                      (if name
                          (cons (list 'defined name stx)
                                (apply append (map collect-info (cddr e))))
                          (apply append (map collect-info (cdr e))))))
                   ((eq? head 'define-values)
                    (let ([names-e (syntax-e (cadr e))])
                      (append
                        (for/list ([n (in-list names-e)])
                          (list 'defined (syntax-e n) n))
                        (apply append (map collect-info (cddr e))))))
                   ((memq head '(define-syntax define-syntaxes)) '())
                   (else '()))))
              ;; Require form
              ((and (pair? e) (eq? (syntax-e (car e)) 'require))
               (map (lambda (n) (list 'defined n stx)) (parse-require-form stx)))
              ;; Struct definition
              ((and (pair? e) (eq? (syntax-e (car e)) 'struct))
               (let* ([name (syntax-e (cadr e))]
                      [base (symbol->string name)])
                 (cons (list 'defined name stx)
                       (list (list 'defined (string->symbol (string-append "make-" base)) stx)
                             (list 'defined (string->symbol (string-append base "?")) stx)))))
              ;; Provide form - skip
              ((and (pair? e) (eq? (syntax-e (car e)) 'provide)) '())
              ;; Module form - skip
              ((and (pair? e) (memq (syntax-e (car e)) '(module module+ submod))) '())
              ;; Other form - collect from sub-expressions
              ((pair? e)
               (let ([head (syntax-e (car e))])
                 (cond
                   ;; Built-in form - just recurse
                   ((and (symbol? head) (is-built-in? head))
                    (apply append (map collect-info (cdr e))))
                   ;; Lambda form - skip binding positions
                   ((memq head '(lambda))
                    (apply append (map collect-info (cddr e))))
                   ;; Let forms - skip binding positions
                   ((memq head '(let let* letrec))
                    (let ([bindings (syntax-e (cadr e))])
                      (append
                        (apply append
                          (for/list ([b (in-list bindings)])
                            (collect-info (cadr (syntax-e b)))))
                        (apply append (map collect-info (cddr e))))))
                   ;; Unknown form - treat head as a reference
                   (else
                    (cons (list 'ref head (car e))
                          (apply append (map collect-info (cdr e))))))))
              (else '())))))

    (define info (collect-info stx))
    (define defined-names
      (list->set (map second (filter (lambda (x) (eq? (first x) 'defined)) info))))
    (define references (filter (lambda (x) (eq? (first x) 'ref)) info))

    (for/list ([ref (in-list references)]
               #:when (not (set-member? defined-names (second ref))))
      (define id (third ref))
      (diagnostic path (or (syntax-line id) 1) (or (syntax-column id) 1)
                  'warning 'reachability/undefined
                  (format "Reference to undefined identifier ~a" (second ref))))))
