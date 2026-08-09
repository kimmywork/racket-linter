#lang racket/base

(require
  racket/contract/base
  racket/syntax
  racket/list
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt"
  (for-syntax racket/base))

(provide module/require-provide)

;; File-level require/provide consistency. Cross-file resolution belongs to the
;; project graph; this rule only checks that provided names have local defs.
(define-rule module/require-provide
  #:id 'module/require-provide
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (define (provided-specs value)
      (define e (syntax-e value))
      (cond
        [(symbol? e) (list (list e value))]
        [(and (pair? e) (eq? (syntax-e (car e)) 'only-in))
         (for/list ([name (in-list (cddr e))])
           (list (syntax-e name) name))]
        [(and (pair? e) (eq? (syntax-e (car e)) 'rename-out))
         (for/list ([mapping (in-list (cdr e))]
                    #:when (pair? (syntax-e mapping)))
           (define mapping-e (syntax-e mapping))
           (list (syntax-e (second mapping-e)) (second mapping-e)))]
        [(and (pair? e) (eq? (syntax-e (car e)) 'struct-out)
              (pair? (cdr e)))
         (list (list (syntax-e (second e)) (second e)))]
        [else '()]))
    (define (provided-ids value)
      (cond
        [(not (syntax? value)) '()]
        [else
         (define e (syntax-e value))
         (if (and (pair? e) (eq? (syntax-e (car e)) 'provide))
             (apply append (map provided-specs (cdr e)))
             (if (pair? e)
                 (apply append (map provided-ids e))
                 '()))]))
    (define (defined-names value)
      (if (not (syntax? value))
          '()
          (let ([e (syntax-e value)])
            (if (not (pair? e))
                '()
                (let* ([head (syntax-e (car e))]
                       [own
                        (cond
                          [(eq? head 'define)
                           (let ([target (second e)])
                             (list
                              (if (pair? (syntax-e target))
                                  (syntax-e (car (syntax-e target)))
                                  (syntax-e target))))]
                          [(eq? head 'define-values)
                           (map syntax-e (syntax-e (second e)))]
                          [(eq? head 'struct)
                           (let ([name (syntax-e (second e))])
                             (list name
                                   (string->symbol
                                    (string-append "make-" (symbol->string name)))
                                   (string->symbol
                                    (string-append (symbol->string name) "?"))))]
                          [else '()])]
                       [nested
                        (if (memq head '(provide require quote quasiquote syntax))
                            '()
                            (apply append (map defined-names e)))])
                  (append own nested))))))
    (define provided (provided-ids stx))
    (define defined (remove-duplicates (defined-names stx)))
    (for/list ([entry (in-list provided)]
               #:unless (member (first entry) defined))
      (define id (second entry))
      (diagnostic path
                  (or (syntax-line id) 1)
                  (or (syntax-column id) 1)
                  'warning
                  'module/require-provide
                  (format "Provided identifier ~a has no local definition"
                          (first entry))))))
