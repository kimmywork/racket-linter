#lang racket/base

;; Conservative abstract interpretation for expanded Racket syntax.
;; Environments retain lexical identifier syntax objects, so shadowed names do
;; not accidentally share one abstract value.

(require
  racket/list
  racket/port
  racket/string
  racket/contract/base
  "diagnostic.rkt")

(provide
 (contract-out
  [analyze-abstract (-> syntax? path-string? (listof diagnostic?))]))

(struct abstract-top () #:transparent)
(struct abstract-bottom (reason source) #:transparent)
(struct abstract-num (value) #:transparent)
(struct abstract-str (value) #:transparent)
(struct abstract-sym (value) #:transparent)
(struct abstract-bool (value) #:transparent)
(struct abstract-lst (element) #:transparent)
(struct abstract-pair (car-type cdr-type) #:transparent)
(struct abstract-vector (element) #:transparent)
(struct abstract-values (values) #:transparent)
(struct abstract-proc
  (source min-arity max-arity rest? params rest-param body env)
  #:transparent)
(struct abstract-union (values) #:transparent)
(struct binding (id value) #:transparent)

(define T (abstract-top))
(define (T? value) (abstract-top? value))
(define (bottom reason [source #f]) (abstract-bottom reason source))
(define (bottom? value) (abstract-bottom? value))

(define (parts-of stx)
  (and (syntax? stx) (syntax->list stx)))

(define (head-name stx)
  (define parts (parts-of stx))
  (and parts (pair? parts) (identifier? (first parts))
       (syntax-e (first parts))))

(define (body-parts parts start)
  (drop parts start))

(define (identifier-name id)
  (and (identifier? id) (syntax-e id)))

;; Environment entries retain lexical identity, not just the printed name.
(define (make-env) '())
(define (env-ref env id)
  (or (for/first ([entry (in-list env)]
                  #:when (free-identifier=? id (binding-id entry)))
        (binding-value entry))
      (builtin-value id)
      T))

(define (env-set env id value)
  (cons (binding id value)
        (filter (lambda (entry)
                  (not (free-identifier=? id (binding-id entry))))
                env)))

(define (env-set-many env ids value)
  (for/fold ([result env]) ([id (in-list ids)])
    (env-set result id value)))

(define primitive-arities
  (hash
   '+ (list 0 #f #t) '- (list 1 #f #t) '* (list 0 #f #t) '/ (list 1 #f #t)
   '= (list 2 #f #t) '< (list 2 #f #t) '> (list 2 #f #t)
   '<= (list 2 #f #t) '>= (list 2 #f #t) 'add1 (list 1 1 #f)
   'sub1 (list 1 1 #f) 'abs (list 1 1 #f) 'max (list 1 #f #t)
   'min (list 1 #f #t) 'zero? (list 1 1 #f) 'positive? (list 1 1 #f)
   'negative? (list 1 1 #f) 'number? (list 1 1 #f)
   'string? (list 1 1 #f) 'symbol? (list 1 1 #f)
   'boolean? (list 1 1 #f) 'pair? (list 1 1 #f) 'null? (list 1 1 #f)
   'list? (list 1 1 #f) 'vector? (list 1 1 #f) 'not (list 1 1 #f)
   'cons (list 2 2 #f) 'car (list 1 1 #f) 'cdr (list 1 1 #f)
   'list (list 0 #f #t) 'list* (list 1 #f #t)
   'make-vector (list 1 2 #f) 'vector (list 0 #f #t)
   'vector-ref (list 2 2 #f) 'vector-length (list 1 1 #f)
   'values (list 0 #f #t) 'void (list 0 0 #f)
   'raise (list 1 2 #f) 'error (list 1 #f #t)))

(define (builtin-value id)
  (define name (identifier-name id))
  (define arity (and name (hash-ref primitive-arities name #f)))
  (and arity
       (abstract-proc id (first arity) (second arity) (third arity)
                      '() #f '() '())))

(define (join-values . values)
  (define flattened
    (append-map (lambda (value)
                  (if (abstract-union? value)
                      (abstract-union-values value)
                      (list value)))
                values))
  (define known (filter (lambda (value) (not (bottom? value))) flattened))
  (cond
    [(null? known) (or (findf bottom? flattened) T)]
    [(ormap T? known) T]
    [(and (pair? known) (andmap abstract-num? known)) (abstract-num #f)]
    [(and (pair? known) (andmap abstract-str? known)) (abstract-str #f)]
    [(and (pair? known) (andmap abstract-bool? known)
          (apply = (map abstract-bool-value known)))
     (first known)]
    [else
     (define distinct (remove-duplicates known equal?))
     (if (= (length distinct) 1)
         (first distinct)
         (abstract-union distinct))]))

(define (known-bool value)
  (if (abstract-bool? value) (abstract-bool-value value) 'unknown))

(define (arity-valid? proc actual)
  (and (>= actual (abstract-proc-min-arity proc))
       (or (not (abstract-proc-max-arity proc))
           (<= actual (abstract-proc-max-arity proc)))))

(define (arity-message proc actual)
  (define min (abstract-proc-min-arity proc))
  (define max (abstract-proc-max-arity proc))
  (if (and max (= min max))
      (format "procedure expects ~a arguments, given ~a" min actual)
      (format "procedure expects at least ~a arguments, given ~a" min actual)))

(define (all-number-or-top? values)
  (andmap (lambda (value) (or (T? value) (abstract-num? value))) values))

(define (apply-predicate name args source)
  (if (not (= (length args) 1))
      (bottom (format "~a requires exactly 1 argument" name) source)
      (let ([arg (first args)])
        (cond
          [(eq? name 'number?) (abstract-bool (or (T? arg) (abstract-num? arg)))]
          [(eq? name 'string?) (abstract-bool (or (T? arg) (abstract-str? arg)))]
          [(eq? name 'symbol?) (abstract-bool (or (T? arg) (abstract-sym? arg)))]
          [(eq? name 'boolean?) (abstract-bool (or (T? arg) (abstract-bool? arg)))]
          [(eq? name 'pair?) (abstract-bool (or (T? arg) (abstract-pair? arg)))]
          [(eq? name 'null?) (abstract-bool (or (T? arg) #f))]
          [(eq? name 'list?) (abstract-bool (or (T? arg) (abstract-lst? arg)))]
          [(eq? name 'vector?) (abstract-bool (or (T? arg) (abstract-vector? arg)))]
          [(memq name '(zero? positive? negative?))
           (if (and (abstract-num? arg) (number? (abstract-num-value arg)))
               (abstract-bool
                (case name
                  [(zero?) (zero? (abstract-num-value arg))]
                  [(positive?) (positive? (abstract-num-value arg))]
                  [else (negative? (abstract-num-value arg))]))
               (abstract-bool #f))]
          [else T]))))

(define (apply-primitive name args source)
  (define proc (builtin-value (datum->syntax #f name)))
  (define argc (length args))
  (cond
    [(not (arity-valid? proc argc)) (bottom (arity-message proc argc) source)]
    [(memq name '(number? string? symbol? boolean? pair? null? list? vector?
                  zero? positive? negative?))
     (apply-predicate name args source)]
    [(memq name '(+ - * / add1 sub1 abs max min))
     (if (all-number-or-top? args)
         (abstract-num #f)
         (bottom (format "~a requires numeric arguments" name) source))]
    [(memq name '(= < > <= >=))
     (if (all-number-or-top? args)
         (abstract-bool #f)
         (bottom (format "~a requires numeric arguments" name) source))]
    [(eq? name 'not)
     (if (abstract-bool? (first args))
         (abstract-bool (not (abstract-bool-value (first args))))
         (abstract-bool #f))]
    [(eq? name 'cons) (abstract-pair (first args) (second args))]
    [(eq? name 'car)
     (cond
       [(abstract-pair? (first args)) (abstract-pair-car-type (first args))]
       [(abstract-lst? (first args)) T]
       [(bottom? (first args)) (first args)]
       [(T? (first args)) T]
       [else (bottom "car requires a pair or list" source)])]
    [(eq? name 'cdr)
     (cond
       [(abstract-pair? (first args)) (abstract-pair-cdr-type (first args))]
       [(abstract-lst? (first args)) (abstract-lst (abstract-lst-element (first args)))]
       [(bottom? (first args)) (first args)]
       [(T? (first args)) T]
       [else (bottom "cdr requires a pair or list" source)])]
    [(eq? name 'list) (abstract-lst (if (null? args) T (apply join-values args)))]
    [(eq? name 'list*) (abstract-lst T)]
    [(eq? name 'make-vector) (abstract-vector (if (= argc 2) (second args) T))]
    [(eq? name 'vector) (abstract-vector (if (null? args) T (apply join-values args)))]
    [(eq? name 'vector-length)
     (if (or (abstract-vector? (first args)) (T? (first args)))
         (abstract-num #f)
         (bottom "vector-length requires a vector" source))]
    [(eq? name 'vector-ref)
     (if (or (abstract-vector? (first args)) (T? (first args)))
         (if (abstract-vector? (first args)) (abstract-vector-element (first args)) T)
         (bottom "vector-ref requires a vector" source))]
    [(eq? name 'values) (abstract-values args)]
    [(eq? name 'void) T]
    [(memq name '(raise error)) (bottom (format "~a always raises" name) source)]
    [else T]))

(define (bind-formals env params rest-param args)
  (define result
    (for/fold ([result env]) ([param (in-list params)] [arg (in-list args)])
      (env-set result param arg)))
  (if rest-param
      (env-set result rest-param (abstract-lst T))
      result))

(define (eval-body bodies env)
  (for/fold ([value T]) ([body (in-list bodies)])
    (aeval body env)))

(define (lambda-value stx formals bodies env)
  (define fs (parts-of formals))
  (cond
    [(and fs (andmap identifier? fs))
     (abstract-proc stx (length fs) (length fs) #f fs #f bodies env)]
    [(identifier? formals)
     (abstract-proc stx 0 #f #t '() formals bodies env)]
    [else (abstract-proc stx 0 #f #t '() #f bodies env)]))

(define (binding-groups syntax)
  (for/list ([group (in-list (or (parts-of syntax) '()))])
    (or (parts-of group) '())))

(define (eval-binding-form parts env recursive?)
  (define groups (binding-groups (second parts)))
  (define initial
    (if recursive?
        (for/fold ([result env]) ([group (in-list groups)])
          (env-set-many result (if (pair? group) (parts-of (first group)) '()) T))
        env))
  (define result
    (for/fold ([result initial]) ([group (in-list groups)])
      (if (< (length group) 2)
          result
          (let* ([ids (or (parts-of (first group)) '())]
                 [value (aeval (second group) (if recursive? result env))])
            (if (abstract-values? value)
                (for/fold ([r result]) ([id (in-list ids)]
                                         [v (in-list (abstract-values-values value))])
                  (env-set r id v))
                (env-set-many result ids value))))))
  (eval-body (body-parts parts 2) result))

(define (aeval stx env)
  (define parts (parts-of stx))
  (cond
    [(identifier? stx) (env-ref env stx)]
    [(not parts) T]
    [else
     (define head (head-name stx))
     (cond
       [(eq? head '#%top)
        (if (and (pair? (cdr parts)) (identifier? (second parts)))
            (env-ref env (second parts))
            T)]
       [(memq head '(#%plain-lambda lambda))
        (lambda-value stx (second parts) (body-parts parts 2) env)]
       [(eq? head 'define-values)
        (aeval (third parts) env)]
       [(eq? head 'if)
        (define test (aeval (second parts) env))
        (case (known-bool test)
          [(#t) (aeval (third parts) env)]
          [(#f) (aeval (fourth parts) env)]
          [else (join-values (aeval (third parts) env)
                             (aeval (fourth parts) env))])]
       [(eq? head 'let-values) (eval-binding-form parts env #f)]
       [(eq? head 'letrec-values) (eval-binding-form parts env #t)]
       [(eq? head 'quote)
        (define value (syntax->datum (second parts)))
        (cond
          [(number? value) (abstract-num value)]
          [(string? value) (abstract-str value)]
          [(symbol? value) (abstract-sym value)]
          [(boolean? value) (abstract-bool value)]
          [(null? value) (abstract-lst T)]
          [(pair? value) (abstract-pair T T)]
          [else T])]
       [(memq head '(#%app #%plain-app))
        (define proc (aeval (second parts) env))
        (define args (map (lambda (arg) (aeval arg env)) (cddr parts)))
        (define name (identifier-name (second parts)))
        (cond
          [(bottom? proc) proc]
          [(and name (hash-has-key? primitive-arities name))
           (apply-primitive name args stx)]
          [(abstract-proc? proc)
           (if (arity-valid? proc (length args))
               (if (null? (abstract-proc-body proc))
                   T
                   (eval-body
                    (abstract-proc-body proc)
                    (bind-formals (abstract-proc-env proc)
                                  (abstract-proc-params proc)
                                  (abstract-proc-rest-param proc)
                                  args)))
               (bottom (arity-message proc (length args)) stx))]
          [(T? proc) T]
          [else (bottom "application requires a procedure" stx)])]
       [(memq head '(begin #%module-begin)) (eval-body (body-parts parts 1) env)]
       [(eq? head 'module) (eval-body (body-parts parts 3) env)]
       [(eq? head 'set!) (aeval (third parts) env)]
       [else T])]))

(define (source-position path position)
  (define text (call-with-input-file path port->string))
  (define bounded (min position (string-length text)))
  (define line
    (add1 (for/sum ([ch (in-string (substring text 0 bounded))]
                    #:when (char=? ch #\newline))
            1)))
  (define last-newline
    (for/fold ([result -1]) ([index (in-range bounded)]
                             #:when (char=? (string-ref text index) #\newline))
      index))
  (values line (max 0 (- bounded last-newline 1))))

(define (stx-location path stx)
  (define position (syntax-position stx))
  (if position
      (source-position path (sub1 position))
      (values (or (syntax-line stx) 1) (or (syntax-column stx) 0))))

(define (report-bottom diagnostics seen path value)
  (define source (and (bottom? value) (abstract-bottom-source value)))
  (if (and source (not (hash-ref seen source #f)))
      (let-values ([(line col) (stx-location path source)])
        (hash-set! seen source #t)
        (cons (diagnostic path line col 'error 'abstract/type-error
                          (format "Abstract application error: ~a"
                                  (abstract-bottom-reason value)))
              diagnostics))
      diagnostics))

(define (walk-sequential forms env walk)
  (for/fold ([result env]) ([form (in-list forms)])
    (walk form result)))

(define (walk-abstract stx path)
  (define diagnostics '())
  (define seen (make-hasheq))
  (define (walk node env)
    (define parts (parts-of node))
    (cond
      [(not parts) env]
      [else
       (define head (head-name node))
       (cond
         [(eq? head 'module)
          (walk-sequential (body-parts parts 3) env walk)]
         [(eq? head '#%module-begin)
          (walk-sequential (body-parts parts 1) env walk)]
         [(eq? head 'define-values)
          (define value-expr (third parts))
          (set! diagnostics
                (report-bottom diagnostics seen path (aeval value-expr env)))
          (define next (walk value-expr env))
          (env-set-many next (or (parts-of (second parts)) '())
                        (aeval value-expr env))]
         [(memq head '(#%app #%plain-app))
          (set! diagnostics
                (report-bottom diagnostics seen path (aeval node env)))
          (walk-sequential (cdr parts) env walk)]
         [(eq? head 'if)
          (walk (second parts) env)
          (define test (aeval (second parts) env))
          (if (eq? (known-bool test) #t)
              (walk (third parts) env)
              (if (eq? (known-bool test) #f)
                  (walk (fourth parts) env)
                  (begin (walk (third parts) env)
                         (walk (fourth parts) env))))
          env]
         [(memq head '(#%plain-lambda lambda))
          (define params (or (parts-of (second parts)) '()))
          (define body-env (env-set-many env (filter identifier? params) T))
          (for ([body (in-list (body-parts parts 2))])
            (walk body body-env))
          env]
         [(memq head '(let-values letrec-values))
          (define groups (binding-groups (second parts)))
          (define recursive? (eq? head 'letrec-values))
          (define value-env
            (if recursive?
                (for/fold ([result env]) ([group (in-list groups)])
                  (env-set-many result
                                (if (pair? group) (or (parts-of (first group)) '()) '())
                                T))
                env))
          (for ([group (in-list groups)] #:when (>= (length group) 2))
            (walk (second group) value-env))
          (define body-env
            (for/fold ([result value-env]) ([group (in-list groups)])
              (env-set-many result
                            (if (pair? group) (or (parts-of (first group)) '()) '())
                            T)))
          (for ([body (in-list (body-parts parts 2))])
            (walk body body-env))
          env]
         [(memq head '(begin #%module-begin))
          (walk-sequential (body-parts parts 1) env walk)]
         [else env])]))
  (walk stx (make-env))
  (reverse diagnostics))

(define (analyze-abstract stx path)
  (walk-abstract stx path))
