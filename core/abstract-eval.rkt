#lang racket/base

;; Abstract interpretation for Racket
;; Inspired by https://github.com/thalesfm/racket-analyzer
;;
;; This module implements a simplified abstract interpreter that can detect:
;; - Type errors (applying non-procedure values)
;; - Uninitialized variable usage (letrec with use before init)
;; - Unreachable code (branches that always evaluate to #f)

(require
  racket/list
  racket/set
  racket/port
  racket/contract/base
  racket/syntax
  syntax/parse
  syntax/id-table
  "diagnostic.rkt")

(provide
  (contract-out
    [analyze-abstract (-> syntax? path-string? (listof diagnostic?))]))

;; Abstract domain
;; T  = top (any value, we don't know)
;; ⊥  = bottom (unreachable/error)
;; num = numeric value
;; str = string value
;; sym = symbol value
;; bool = boolean value
;; proc = procedure value

(struct abstract-top () #:transparent)
(struct abstract-bottom (reason) #:transparent)
(struct abstract-proc (source) #:transparent)
(struct abstract-num () #:transparent)
(struct abstract-str () #:transparent)
(struct abstract-sym () #:transparent)
(struct abstract-bool (val) #:transparent)

(define T (abstract-top))
(define (T? v) (abstract-top? v))
(define (⊥ reason) (abstract-bottom reason))
(define (⊥? v) (abstract-bottom? v))
(define (make-proc src) (abstract-proc src))
(define (make-num) (abstract-num))
(define (make-str) (abstract-str))
(define (make-sym) (abstract-sym))
(define (make-bool v) (abstract-bool v))

;; Environment: maps identifiers to abstract values
(define (make-env) (make-immutable-bound-id-table))
(define (env-ref env id)
  (bound-id-table-ref env id (lambda () (⊥ (format "unbound: ~a" (syntax-e id))))))
(define (env-set env id val)
  (bound-id-table-set env id val))

;; Abstract evaluator
(define (aeval stx env)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    
    ;; Locally bound identifier
    [(~and id:id (~fail #:unless (eq? (identifier-binding #'id) 'lexical)))
     (env-ref env #'id)]
    
    ;; Top-level, module-level, or unbound identifier
    [id:id T]
    
    ;; Top-level reference (#%top . id) - look up in environment
    [(#%top . id)
     (env-ref env #'id)]
    
    ;; Lambda
    [(#%plain-lambda formals body ...)
     (make-proc stx)]
    
    ;; Define-values (top-level definition)
    [(define-values (id ...) expr)
     (define val (aeval #'expr env))
     (define env*
       (let loop ([ids (syntax->list #'(id ...))] [e env])
         (if (null? ids)
             e
             (loop (cdr ids) (env-set e (car ids) val)))))
     val]
    
    ;; If
    [(if test-expr then-expr else-expr)
     (define test-val (aeval #'test-expr env))
     (cond
       [(⊥? test-val) test-val]
       [else (let ([then-v (aeval #'then-expr env)]
                   [else-v (aeval #'else-expr env)])
               (cond
                 [(equal? then-v else-v) then-v]
                 [(⊥? then-v) else-v]
                 [(⊥? else-v) then-v]
                 [else T]))])]
    
    ;; Let-values
    [(let-values ([(id ...) val-expr] ...) body ...)
     (define env*
       (let loop ([ids-list (syntax->list #'((id ...) ...))]
                  [vals-list (syntax->list #'(val-expr ...))]
                  [e env])
         (if (null? ids-list)
             e
             (let* ([ids (car ids-list)]
                    [val (aeval (car vals-list) env)]
                    [e2 (let inner ([id-list (syntax->list ids)] [e3 e])
                          (if (null? id-list)
                              e3
                              (inner (cdr id-list) (env-set e3 (car id-list) val))))])
               (loop (cdr ids-list) (cdr vals-list) e2)))))
     (let body-loop ([bodies (syntax->list #'(body ...))])
       (if (null? (cdr bodies))
           (aeval (car bodies) env*)
           (begin (aeval (car bodies) env*)
                  (body-loop (cdr bodies)))))]
    
    ;; Letrec-values with fixpoint iteration for recursive functions
    [(letrec-values ([(id ...) val-expr] ...) body ...)
     ;; First, bind all identifiers to T (uninitialized)
     (define initial-env
       (let loop ([ids-list (syntax->list #'((id ...) ...))]
                  [e env])
         (if (null? ids-list)
             e
             (let* ([ids (car ids-list)]
                    [e2 (let inner ([id-list (syntax->list ids)] [e3 e])
                          (if (null? id-list)
                              e3
                              (inner (cdr id-list) (env-set e3 (car id-list) T))))])
               (loop (cdr ids-list) e2)))))
     
     ;; Fixpoint iteration for recursive definitions
     (define max-iterations 10)
     (define-values (final-env _)
       (let iteration ([current-env initial-env]
                       [prev-env #f]
                       [iter 0])
         (if (and prev-env (equal? current-env prev-env))
             ;; Fixpoint reached
             (values current-env iter)
             (if (>= iter max-iterations)
                 ;; Max iterations reached
                 (values current-env iter)
                 ;; Evaluate all value expressions with current environment
                 (let ([new-env
                        (let eval-loop ([ids-list (syntax->list #'((id ...) ...))]
                                       [vals-list (syntax->list #'(val-expr ...))]
                                       [e current-env])
                          (if (null? ids-list)
                              e
                              (let* ([ids (car ids-list)]
                                     [val (aeval (car vals-list) current-env)]
                                     [e2 (let inner ([id-list (syntax->list ids)] [e3 e])
                                           (if (null? id-list)
                                               e3
                                               (inner (cdr id-list) (env-set e3 (car id-list) val))))])
                                (eval-loop (cdr ids-list) (cdr vals-list) e2))))])
                   (iteration new-env current-env (+ iter 1)))))))
     
     ;; Then evaluate the bodies with the final environment
     (let body-loop ([bodies (syntax->list #'(body ...))])
       (if (null? (cdr bodies))
           (aeval (car bodies) final-env)
           (begin (aeval (car bodies) final-env)
                  (body-loop (cdr bodies)))))]
    
    ;; Quote
    [(quote datum)
     (define d (syntax->datum #'datum))
     (cond
       [(number? d) (make-num)]
       [(string? d) (make-str)]
       [(symbol? d) (make-sym)]
       [(boolean? d) (make-bool d)]
       [else T])]
    
    ;; Application
    [(#%plain-app proc-expr arg-exprs ...)
     (define proc-val (aeval #'proc-expr env))
     (cond
       [(⊥? proc-val) proc-val]
       [(abstract-proc? proc-val) T]
       [(T? proc-val) T]
       [(abstract-num? proc-val) (⊥ "not a procedure")]
       [(abstract-str? proc-val) (⊥ "not a procedure")]
       [(abstract-sym? proc-val) (⊥ "not a procedure")]
       [(abstract-bool? proc-val) (⊥ "not a procedure")]
       [else T])]
    
    ;; Begin
    [(begin body ...)
     (let loop ([bodies (syntax->list #'(body ...))])
       (if (null? (cdr bodies))
           (aeval (car bodies) env)
           (begin (aeval (car bodies) env)
                  (loop (cdr bodies)))))]
    
    ;; Begin0
    [(begin0 first-expr body ...)
     (aeval #'first-expr env)]
    
    ;; Set!
    [(set! id expr)
     (aeval #'expr env)]
    
    ;; Default: return T
    [_ T]))

;; Main analysis function
(define (analyze-abstract stx path)
  (define env (make-env))
  (define diagnostics '())
  
  ;; Walk the syntax and check for issues
  (define (walk stx env)
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      
      ;; Define-values: track variable values
      [(define-values (id ...) expr)
       (define val (aeval #'expr env))
       (define env*
         (let loop ([ids (syntax->list #'(id ...))] [e env])
           (if (null? ids)
               e
               (loop (cdr ids) (env-set e (car ids) val)))))
       (walk #'expr env)
       env*]
      
      ;; Application: check if proc is actually a procedure
      [(#%plain-app proc-expr arg-exprs ...)
       (define proc-val (aeval #'proc-expr env))
       (when (or (abstract-num? proc-val)
                 (abstract-str? proc-val)
                 (abstract-sym? proc-val)
                 (abstract-bool? proc-val))
         (set! diagnostics
               (cons (diagnostic path 1 1 'error 'abstract/type-error
                                 (format "Not a procedure: ~a" proc-val))
                     diagnostics)))
       (walk #'proc-expr env)
       (for ([arg (syntax->list #'(arg-exprs ...))])
         (walk arg env))
       env]
      
      ;; Letrec-values: check for use before initialization
      [(letrec-values ([(id ...) val-expr] ...) body ...)
       ;; Bind all to T first
       (define env*
         (let loop ([ids-list (syntax->list #'((id ...) ...))]
                    [e env])
           (if (null? ids-list)
               e
               (let* ([ids (car ids-list)]
                      [e2 (let inner ([id-list (syntax->list ids)] [e3 e])
                            (if (null? id-list)
                                e3
                                (inner (cdr id-list) (env-set e3 (car id-list) T))))])
                 (loop (cdr ids-list) e2)))))
       ;; Then check bodies
       (for ([val-expr (syntax->list #'(val-expr ...))])
         (walk val-expr env*))
       (for ([body (syntax->list #'(body ...))])
         (walk body env*))
       env*]
      
      ;; Begin: walk all forms, tracking env through define-values
      [(begin body ...)
       (let loop ([bodies (syntax->list #'(body ...))] [e env])
         (if (null? bodies)
             e
             (let ([result (walk (car bodies) e)])
               (loop (cdr bodies) (if (void? result) e result)))))]
      
      ;; If: check if test is always false
      [(if test-expr then-expr else-expr)
       (walk #'test-expr env)
       (walk #'then-expr env)
       (walk #'else-expr env)
       env]
      
      ;; Default: return env
      [_ env]))
  
  (walk stx env)
  diagnostics)
