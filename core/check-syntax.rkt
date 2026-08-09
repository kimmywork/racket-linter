#lang racket/base

;; Check-Syntax integration module for racket-linter
;;
;; This module provides integration with DrRacket's check-syntax API
;; for precise detection of unused variables and unused requires.
;;
;; Uses make-traversal for richer binding information including:
;; - Unused variables (syncheck:unused-binder)
;; - Unused requires (syncheck:add-unused-require)
;; - Definition locations
;; - Binding arrows

(require
  drracket/check-syntax
  racket/class
  racket/contract
  racket/port
  racket/path
  racket/string
  racket/list
  racket/set
  syntax/modread
  "diagnostic.rkt")

(provide
  (contract-out
    [check-syntax-analyze (-> path-string? (listof diagnostic?))]))

;; Collector class that implements syncheck-annotations<%>
;; to collect binding information, unused variables, etc.
(define collector%
  (class* object% (syncheck-annotations<%>)
    (init-field src)
    (define diagnostics '())
    (define definitions (make-hash)) ; name -> (list start end)
    (define references (make-hash)) ; name -> (list (list start end) ...)
    (define unused-binders '())
    (define unused-requires '())
    
    (super-new)
    
    ;; Required methods for syncheck-annotations<%>
    (define/public (syncheck:add-definition-source src-obj start end name)
      (hash-set! definitions name (list start end)))
    
    (define/public (syncheck:add-jump-to-definition src-obj start end name def-src def-start def-end)
      (hash-update! references name (lambda (old) (cons (list start end) old)) '()))
    
    (define/public (syncheck:add-jump-to-definition/phase-level+space a b c d e f g) (void))
    (define/public (syncheck:add-require-open-menu a b c d) (void))
    (define/public (syncheck:add-mouse-over-status a b c d) (void))
    (define/public (syncheck:add-arrow/name-dup/pxpy a b c d e f g h i j k l m n) (void))
    (define/public (syncheck:add-tail-arrow a b c d) (void))
    (define/public (syncheck:add-rename-menu a b c d) (void))
    (define/public (syncheck:add-background-color a b c d) (void))
    (define/public (syncheck:add-prefixed-require-reference a b c d e) (void))
    (define/public (syncheck:add-arrow a b c d e f g h i j) (void))
    (define/public (syncheck:find-source-object a) a)
    (define/public (syncheck:add-text-type a b c d) (void))
    (define/public (syncheck:add-definition-target a b c d e) (void))
    (define/public (syncheck:color-range a b c d) (void))
    (define/public (syncheck:add-docs-menu a b c d e f g h) (void))
    (define/public (syncheck:add-id-set a b c d e f) (void))
    (define/public (syncheck:add-arrow/name-dup a b c d e f g h i j) (void))
    (define/public (syncheck:add-definition-target/phase-level+space a b c d e f) (void))
    
    ;; Called when unused variables are found
    (define/public (syncheck:unused-binder src-obj start end)
      (set! unused-binders (cons (list start end) unused-binders)))
    
    ;; Called when unused requires are found
    (define/public (syncheck:add-unused-require src-obj start end)
      (set! unused-requires (cons (list start end) unused-requires)))
    
    ;; Get collected data
    (define/public (get-diagnostics) diagnostics)
    (define/public (get-definitions) definitions)
    (define/public (get-references) references)
    (define/public (get-unused-binders) unused-binders)
    (define/public (get-unused-requires) unused-requires)))

;; Analyze a file using DrRacket's check-syntax API.
;;
;; This function uses make-traversal to get detailed information about
;; the file's syntax, including:
;; - Unused variables (syncheck:unused-binder)
;; - Unused requires (syncheck:add-unused-require)
;; - Definition locations
;; - Binding arrows
;;
;; Returns a list of diagnostics for any issues found.
(define (check-syntax-analyze path)
  (define ns (make-base-namespace))
  (define src-dir (path-only (string->path path)))
  (define collector (new collector% [src path]))
  
  (define-values (add-syntax done)
    (make-traversal ns src-dir))
  
  (parameterize ([current-load-relative-directory src-dir]
                 [current-namespace ns]
                 [current-annotations collector])
    (define stx
      (with-handlers ([exn:fail? (lambda (exn) #f)])
        (call-with-input-file path
          (lambda (in)
            (with-module-reading-parameterization
              (lambda () (read-syntax path in)))))))
    
    (when (syntax? stx)
      (define expanded-stx
        (with-handlers ([exn:fail? (lambda (exn) #f)])
          (parameterize ([current-output-port (open-output-nowhere)])
            (expand stx))))
      
      (when (syntax? expanded-stx)
        (add-syntax expanded-stx)
        (done))))
  
  ;; Collect diagnostics from the collector
  (define diagnostics '())
  
  ;; Add unused binder diagnostics
  (for ([binder (in-list (send collector get-unused-binders))])
    (define start (first binder))
    (define end (second binder))
    (set! diagnostics
          (cons (diagnostic path 1 1 'info 'check-syntax/unused-variable
                            (format "Variable at position ~a-~a is defined but never used" start end))
                diagnostics)))
  
  ;; Add unused require diagnostics
  (for ([req (in-list (send collector get-unused-requires))])
    (define start (first req))
    (define end (second req))
    (set! diagnostics
          (cons (diagnostic path 1 1 'info 'check-syntax/unused-require
                            (format "Unused require at position ~a-~a" start end))
                diagnostics)))
  
  diagnostics)
