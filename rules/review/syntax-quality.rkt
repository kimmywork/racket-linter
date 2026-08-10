#lang racket/base

;; Syntax-aware compatibility checks for the surface rules implemented by
;; raco review. This rule runs on the original syntax, so paren shape,
;; lexical nesting, and source locations remain available.

(require
  racket/list
  racket/string
  racket/contract/base
  "../../core/rule.rkt"
  "../../core/diagnostic.rkt")

(provide review/syntax-quality)

(struct scope (parent names) #:transparent)

(define (parts-of stx)
  (define parts (and (syntax? stx) (syntax->list stx)))
  (and (pair? parts) parts))

(define (head-name parts)
  (and (pair? parts) (identifier? (first parts)) (syntax-e (first parts))))

(define (name-of stx)
  (and (identifier? stx) (syntax-e stx)))

(define (diag path stx severity rule-id message)
  (diagnostic path (or (syntax-line stx) 1) (or (syntax-column stx) 0)
              severity rule-id message))

(define (binding-ids formals)
  (filter identifier? (or (parts-of formals) '())))

(define (scope-contains? current name)
  (cond
    [(not current) #f]
    [(hash-has-key? (scope-names current) name) current]
    [(scope-parent current) => (lambda (parent) (scope-contains? parent name))]
    [else #f]))

(define (bind! current id path diagnostics)
  (define name (name-of id))
  (if (not name)
      diagnostics
      (cond
        [(hash-has-key? (scope-names current) name)
         (cons (diag path id 'error 'review/already-defined
                    (format "identifier '~a' is already defined" name))
               diagnostics)]
        [(scope-contains? (scope-parent current) name)
         (hash-set! (scope-names current) name #t)
         (cons (diag path id 'warning 'review/shadowing
                    (format "identifier '~a' shadows an earlier binding" name))
               diagnostics)]
        [else
         (hash-set! (scope-names current) name #t)
         diagnostics])))

(define (bracket-warning path stx message diagnostics)
  (if (eq? (syntax-property stx 'paren-shape) #\[)
      diagnostics
      (cons (diag path stx 'warning 'review/bracket-shape message) diagnostics)))

(define (simple-form-name stx)
  (cond
    [(identifier? stx) (symbol->string (syntax-e stx))]
    [(string? (syntax-e stx)) (syntax-e stx)]
    [else (format "~s" (syntax->datum stx))]))

(define (check-require-order path parts diagnostics)
  (define specs (rest parts))
  (define keys (map simple-form-name specs))
  (define ordered (sort keys string<?))
  (if (equal? keys ordered)
      diagnostics
      (cons (diag path (first parts) 'info 'review/require-order
                 "require arguments should be sorted alphabetically")
            diagnostics)))

(define (check-provide-order path parts diagnostics)
  (define specs (rest parts))
  (define keys (map simple-form-name specs))
  (define ordered (sort keys string<?))
  (if (equal? keys ordered)
      diagnostics
      (cons (diag path (first parts) 'info 'review/provide-order
                 "provide arguments should be sorted alphabetically")
            diagnostics)))

(define (collect-defined! node defined)
  (define parts (parts-of node))
  (when parts
    (case (head-name parts)
      [(define-values)
       (for ([id (in-list (or (parts-of (second parts)) '()))]
             #:when (identifier? id))
         (hash-set! defined (name-of id) #t))]
      [(define)
       (define signature (second parts))
       (hash-set! defined
                  (or (name-of signature)
                      (and (parts-of signature)
                           (name-of (first (parts-of signature)))))
                  #t)]
      [(struct)
       (when (and (> (length parts) 1) (identifier? (second parts)))
         (define name (name-of (second parts)))
         (hash-set! defined name #t)
         (hash-set! defined (string->symbol (format "~a?" name)) #t)
         (define fields
           (and (> (length parts) 2) (parts-of (third parts))))
         (for ([field (in-list (or fields '()))]
               #:when (identifier? field))
           (hash-set! defined
                      (string->symbol (format "~a-~a" name (name-of field)))
                      #t)))]
      [else
       (for ([part (in-list parts)])
         (collect-defined! part defined))])))

(define (check-provides path parts defined diagnostics)
  (for/fold ([result diagnostics]) ([spec (in-list (rest parts))]
                                    #:when (identifier? spec))
    (if (hash-has-key? defined (name-of spec))
        result
        (cons (diag path spec 'warning 'review/provided-but-not-defined
                   (format "identifier '~a' provided but not defined" (name-of spec)))
              result))))

(define (scan-bindings node current path diagnostics)
  (define parts (parts-of node))
  (cond
    [(not parts) diagnostics]
    [else
     (define head (head-name parts))
     (case head
       [(define-values)
        (define result
          (for/fold ([result diagnostics]) ([id (in-list (or (parts-of (second parts)) '()))]
                                             #:when (identifier? id))
            (bind! current id path result)))
        (scan-bindings (third parts) current path result)]
       [(define)
        (define signature (second parts))
        (define signature-parts (parts-of signature))
        (define name
          (if (identifier? signature) signature (and signature-parts (first signature-parts))))
        (define result (if (identifier? name) (bind! current name path diagnostics) diagnostics))
        (if signature-parts
            (let ([inner (scope current (make-hash))])
              (define with-args
                (for/fold ([r result]) ([arg (in-list (rest signature-parts))]
                                         #:when (identifier? arg))
                  (bind! inner arg path r)))
              (for/fold ([r with-args]) ([body (in-list (drop parts 2))])
                (scan-bindings body inner path r)))
            (for/fold ([r result]) ([body (in-list (drop parts 2))])
              (scan-bindings body current path r)))]
       [(lambda)
        (define inner (scope current (make-hash)))
        (define with-args
          (for/fold ([r diagnostics]) ([arg (in-list (binding-ids (second parts)))])
            (bind! inner arg path r)))
        (for/fold ([r with-args]) ([body (in-list (drop parts 2))])
          (scan-bindings body inner path r))]
       [(let let* letrec)
        (define groups (or (parts-of (second parts)) '()))
        (define result
          (for/fold ([r diagnostics]) ([group (in-list groups)])
            (define group-parts (parts-of group))
            (if (and group-parts (pair? group-parts))
                (scan-bindings (last group-parts) current path
                               (if (and (pair? (first group-parts))
                                        (identifier? (first (parts-of (first group-parts)))) )
                                   r
                                   r))
                r)))
        (define inner (scope current (make-hash)))
        (define with-bindings
          (for/fold ([r result]) ([group (in-list groups)])
            (define group-parts (parts-of group))
            (if (and group-parts (pair? group-parts)
                     (identifier? (first group-parts)))
                (bind! inner (first group-parts) path r)
                r)))
        (for/fold ([r with-bindings]) ([body (in-list (drop parts 2))])
          (scan-bindings body inner path r))]
       [else
        (for/fold ([r diagnostics]) ([child (in-list (rest parts))])
          (scan-bindings child current path r))])]))

(define (scan-shape node path defined diagnostics)
  (define parts (parts-of node))
  (cond
    [(not parts) diagnostics]
    [else
     (define head (head-name parts))
     (cond
       [(memq head '(quote quasiquote syntax quasisyntax)) diagnostics]
       [else
        (define findings
          (cond
            [(eq? head 'if)
             (cond
               [(not (= (length parts) 4))
                (list
                 (diag path (first parts) 'error 'review/if-arity
                       "if expressions must contain one then-branch and one else-branch"))]
               [else
                (define then-parts (parts-of (third parts)))
                (define else-parts (parts-of (fourth parts)))
                (define nested
                  (if (and then-parts (memq (head-name then-parts) '(begin let)))
                      (diag path (third parts) 'warning 'review/if-shape
                           "use a cond expression instead of nesting begin or let inside an if")
                      '()))
                (if (and else-parts (eq? (head-name else-parts) 'if))
                    (cons (diag path (fourth parts) 'warning 'review/if-shape
                                "flatten nested if expressions into a cond expression")
                          (if (null? nested) '() (list nested)))
                    (if (null? nested) '() (list nested)))])]
            [(memq head '(let let* letrec))
             (define bodies (drop parts 2))
             (define result (if (null? bodies)
                                (list (diag path (first parts) 'error 'review/empty-body
                                           "let forms must contain at least one body expression"))
                                '()))
             (append result
                     (for/list ([group (in-list (or (parts-of (second parts)) '()))]
                                #:unless (eq? (syntax-property group 'paren-shape) #\[))
                       (diag path group 'warning 'review/bracket-shape
                             "bindings within a let should be surrounded by square brackets")))]
            [(eq? head 'cond)
             (define clauses (rest parts))
             (append
              (if (and (pair? clauses)
                       (not (eq? (head-name (parts-of (last clauses))) 'else)))
                  (list (diag path (first parts) 'warning 'review/cond-shape
                              "this cond expression does not have an else clause"))
                  '())
              (for/list ([clause (in-list clauses)]
                         #:unless (eq? (syntax-property clause 'paren-shape) #\[))
                (diag path clause 'warning 'review/bracket-shape
                      "this cond clause should be surrounded by square brackets")))]
            [(eq? head 'case)
             (for/list ([clause (in-list (drop parts 2))]
                        #:when (let ([cp (parts-of clause)])
                                 (and cp (pair? cp)
                                      (let ([p (parts-of (first cp))])
                                        (and p (memq (head-name p) '(quote quasiquote)))))))
               (diag path clause 'error 'review/case-shape
                     "case clause must use constants directly, not quoted constants"))]
            [(memq head '(match match-lambda match-lambda*))
             (for/list ([clause (in-list (if (eq? head 'match) (drop parts 2) (rest parts)))]
                        #:when
                        (let* ([clause-parts (parts-of clause)]
                               [pattern (and clause-parts (first clause-parts))]
                               [pattern-name (and pattern (name-of pattern))])
                          (or (eq? pattern-name 'else)
                              (member pattern-name '(null empty)))))
               (define pattern (first (parts-of clause)))
               (diag path pattern 'error 'review/match-shape
                     (if (eq? (name-of pattern) 'else)
                         "use _ instead of else in the fallthrough case of a match expression"
                         "use '() for match pattern instead of null or empty")))]
            [(eq? head 'lambda)
             (if (member '_ (map name-of (binding-ids (second parts))))
                 (list (diag path (second parts) 'warning 'review/lambda-shape
                            "avoid lambda _; use a named argument or a pattern"))
                 '())]
            [(eq? head 'false/c)
             (list (diag path (first parts) 'warning 'review/boolean-shape
                        "prefer #f over false/c"))]
            [(eq? head 'require) (check-require-order path parts '())]
            [(eq? head 'provide) (append (check-provide-order path parts '())
                                          (check-provides path parts defined '()))]
            [(and (identifier? (first parts)) (eq? (name-of (first parts)) 'false/c))
             (list (diag path (first parts) 'warning 'review/boolean-shape
                        "prefer #f over false/c"))]
            [else '()]))
        (define result (append diagnostics findings))
        (for/fold ([acc result]) ([child (in-list (rest parts))])
          (scan-shape child path defined acc))])]))

(define-rule review/syntax-quality
  #:id 'review/syntax-quality
  #:severity 'warning
  #:config-keys (hash 'enabled #f)
  #:layer 'syntax
  (lambda (stx path config)
    (define defined (make-hash))
    (collect-defined! stx defined)
    (define binding-results (scan-bindings stx (scope #f (make-hash)) path '()))
    (scan-shape stx path defined binding-results)))
