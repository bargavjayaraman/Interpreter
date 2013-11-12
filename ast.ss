#lang racket
(require eopl)

(provide op?)
(provide ast?)
(provide ast-datatype)

(provide num-ast)
(provide num-ast?)
(provide num-ast-datum)

(provide bool-ast)
(provide bool-ast?)
(provide bool-ast-datum)

(provide id-ref-ast)
(provide id-ref-ast?)
(provide id-ref-ast-id)

(provide if-ast)
(provide if-ast?)
(provide if-ast-test)
(provide if-ast-then)
(provide if-ast-else)

(provide app-ast)
(provide app-ast?)
(provide app-ast-rator)
(provide app-ast-rands)

(provide and-ast)
(provide and-ast?)
(provide and-asts)

(provide or-ast)
(provide or-ast?)
(provide or-asts)

(provide cond-ast)
(provide cond-ast?)
(provide cond-clauses)

(provide clause-datatype)
(provide clause?)
(provide clause)
(provide clause-test)
(provide clause-then)

(provide let-ast)
(provide let-ast?)
(provide let-ast-binds)
(provide let-ast-body)

(provide bind-datatype)
(provide bind)
(provide bind?)
(provide bind-id)
(provide bind-ast)

(provide fn-ast)
(provide fn-ast?)
(provide fn-ast-formals)
(provide fn-ast-body)

(provide fix-ast)
(provide fix-ast?)
(provide fix-ast-fbinds)
(provide fix-ast-body)

(provide fbind-datatype)
(provide fbind)
(provide fbind?)
(provide fbind-fname)
(provide fbind-formals)
(provide fbind-body)

(provide new-ref-ast)
(provide new-ref-ast?)
(provide new-ref-ast-val)

(provide deref-ast)
(provide deref-ast?)
(provide deref-ast-var)

(provide setref-ast)
(provide setref-ast?)
(provide setref-ast-var)
(provide setref-ast-val)

(provide seq-ast)
(provide seq-ast?)
(provide seq-ast-body)

;; List of operators available in the interpreter
(define operators '(+ - * / >= <= > < = not sub1 add1 0?))

;; Datatype definitions
(define-datatype ast-datatype ast?
  [num-ast (datum number?)]
  [bool-ast (datum boolean?)]
  [id-ref-ast (sym symbol?)]
  [if-ast (test ast?) (then ast?) (else ast?)]
  [app-ast (rator ast?) (rands (list-of ast?))]
  [and-ast (asts (list-of ast?))]
  [or-ast (asts (list-of ast?))]
  [cond-ast (clauses (list-of clause?))]
  [let-ast (binds (list-of bind?)) (body ast?)]
  [fn-ast (formals (list-of symbol?)) (body ast?)]
  [fix-ast (fbinds (list-of fbind?)) (body ast?)]
  [new-ref-ast (val ast?)]
  [deref-ast (var ast?)]
  [setref-ast (var ast?) (val ast?)]
  [seq-ast (body (list-of ast?))]
  )

;; Datatype definitions
(define-datatype clause-datatype clause?
  [clause (test ast?) (then ast?)])

;; Datatype definitions
(define-datatype bind-datatype bind?
  [bind (b-id id-ref-ast?) (b-ast ast?)])

;; Datatype definitions
(define-datatype fbind-datatype fbind?
  [fbind (fname symbol?) (formals (list-of symbol?)) (body ast?)])

;; op? :: thing? -> boolean?
(define op? (lambda (thing) (cons? (member thing operators)) ))

;; num-ast? :: ast? -> boolean?
(define num-ast? 
  (lambda (ast) 
    (cases ast-datatype ast
    [num-ast (datum) #t]
    [else #f])))

;; num-ast-datum :: ast? -> number?
(define num-ast-datum
  (lambda (ast)
    (cases ast-datatype ast
      [num-ast (ast) ast]
      [else (error "Incompatible ast")])))

;; bool-ast? :: ast? -> boolean?
(define bool-ast? 
  (lambda (ast) 
    (cases ast-datatype ast
    [bool-ast (datum) #t]
    [else #f])))

;; bool-ast-datum :: ast? -> boolean?
(define bool-ast-datum
  (lambda (ast)
    (cases ast-datatype ast
      [bool-ast (ast) ast]
      [else (error "Incompatible ast")])))

;; id-ref-ast? :: ast? -> boolean?
(define id-ref-ast? 
  (lambda (ast) 
    (cases ast-datatype ast
    [id-ref-ast (datum) #t]
    [else #f])))

;; id-ref-ast-id :: ast? -> symbol?
(define id-ref-ast-id
  (lambda (ast)
    (cases ast-datatype ast
      [id-ref-ast (ast) ast]
      [else (error "Incompatible ast")])))

;; if-ast? :: ast? -> boolean?
(define if-ast?
  (lambda (ast)
    (cases ast-datatype ast
      [if-ast (test then else) #t]
      [else #f])))

;; if-ast-test :: ast? -> ast?
(define if-ast-test
  (lambda (ast)
    (cases ast-datatype ast
      [if-ast (test then else) test]
      [else (error "Incompatible ast")])))

;; if-ast-then :: ast? -> ast?
(define if-ast-then
  (lambda (ast)
    (cases ast-datatype ast
      [if-ast (test then else) then]
      [else (error "Incompatible ast")])))

;; if-ast-else :: ast? -> ast?
(define if-ast-else
  (lambda (ast)
    (cases ast-datatype ast
      [if-ast (test then else) else]
      [else (error "Incompatible ast")])))

;; app-ast? :: ast? -> boolean?
(define app-ast?
  (lambda (ast) 
    (cases ast-datatype ast
      [app-ast (rator rands) #t]
      [else #f])))

;; app-ast-rator :: ast? -> op?
(define app-ast-rator
  (lambda(ast)
    (cases ast-datatype ast
      [app-ast (rator rands) rator]
      [else (error "Incompatible ast")])))

;; app-ast-rands :: ast? -> list-of ast?
(define app-ast-rands
  (lambda(ast)
    (cases ast-datatype ast
      [app-ast (rator rands) rands]
      [else (error "Incompatible ast")])))

;; and-ast? :: ast? -> boolean?
(define and-ast?
  (lambda (ast)
    (cases ast-datatype ast
      [and-ast (ast) #t]
      [else #f])))

;; and-asts :: ast? -> list-of ast?
(define and-asts
  (lambda (ast)
    (cases ast-datatype ast
      [and-ast (ast) ast]
      [else (error "Incompatible types")])))

;; or-ast? :: ast? -> boolean?
(define or-ast?
  (lambda (ast)
    (cases ast-datatype ast
      [or-ast (ast) #t]
      [else #f])))

;; or-asts :: ast? -> list-of ast?
(define or-asts
  (lambda (ast)
    (cases ast-datatype ast
      [or-ast (ast) ast]
      [else (error "Incompatible types")])))


;; cond-ast? :: ast? -> boolean?
(define cond-ast? 
  (lambda (ast)
    (cases ast-datatype ast
      [cond-ast (ast) #t ]
      [else #f])))

;; cond-clauses :: ast? -> list-of clause?
(define cond-clauses
  (lambda (ast)
    (cases ast-datatype ast
      [cond-ast (ast) ast]
      [else (error "Incompatible types")])))


;; clause-test :: clause? -> ast?
(define clause-test
  (lambda (ast)
    (cases clause-datatype ast
      [clause (test then) test]
      [else (error "Imcompatible types")])))

;; clause-then :: clause? -> ast?
(define clause-then
  (lambda (ast)
    (cases clause-datatype ast
      [clause (test then) then]
      [else (error "Imcompatible types")])))

;; let-ast? :: ast? -> boolean?
(define let-ast? 
  (lambda (ast)
    (cases ast-datatype ast
      [let-ast (binds body) #t]
      [else #f])))

;; let-ast-binds :: ast? -> list-of bind?
(define let-ast-binds
  (lambda (ast)
    (cases ast-datatype ast
      [let-ast (binds body) binds]
      [else (error "Incompatible types")])))

;; let-ast-body :: ast? -> ast?
(define let-ast-body
  (lambda (ast)
    (cases ast-datatype ast
      [let-ast (binds body) body]
      [else (error "Incompatible types")])))

;; bind-id :: ast? -> id-ref?
(define bind-id
  (lambda (ast)
    (cases bind-datatype ast
    [bind (b-id b-ast) b-id]
    [else (error "Incompatible types")])))

;; bind-ast :: ast? -> ast?
(define bind-ast
  (lambda (ast)
    (cases bind-datatype ast
    [bind (b-id b-ast) b-ast]
    [else (error "Incompatible types")])))

;; fn-ast? :: ast? -> boolean?
(define fn-ast? 
  (lambda (ast) 
    (cases ast-datatype ast
      [fn-ast (formals body) #t]
      [else #f])))

;; fn-ast-formals :: ast? -> list-of id-ref-ast?
(define fn-ast-formals
  (lambda (ast)
    (cases ast-datatype ast
      [fn-ast (formals body) formals]
      [else (error "Incompatible types")])))

;; fn-ast-body :: ast? -> ast?
(define fn-ast-body
  (lambda (ast)
    (cases ast-datatype ast
      [fn-ast (formals body) body]
      [else (error "Incompatible types")])))

;; fix-ast? :: ast? -> boolean?
(define fix-ast?
  (lambda (ast) 
    (cases ast-datatype ast
      [fix-ast (formals body) #t]
      [else #f])))

;; fix-ast-binds :: ast? -> list-of fbind?
(define fix-ast-fbinds
  (lambda (ast) 
    (cases ast-datatype ast
      [fix-ast (fbinds body) fbinds]
      [else (error "Incompatible types")])))

;; fix-ast-body :: ast? -> list-of ast?
(define fix-ast-body
  (lambda (ast) 
    (cases ast-datatype ast
      [fix-ast (fbinds body) body]
      [else (error "Incompatible types")])))

;; fbind-name :: fbind? -> id-ref-ast?
(define fbind-fname
  (lambda (ast)
    (cases fbind-datatype ast
      [fbind (fname formals body) fname]
      [else (error "Incompatible types")])))

;; fbind-formals :: fbind? -> list-of denotable-values?
(define fbind-formals
  (lambda (ast)
    (cases fbind-datatype ast
      [fbind (fname formals body) formals]
      [else (error "Incompatible types")])))

;; find-body :: fbind? -> ast?
(define fbind-body
  (lambda (ast)
    (cases fbind-datatype ast
      [fbind (fname formals body) body]
      [else (error "Incompatible types")])))

;; new-ref-ast? :: ast? -> boolean?
(define new-ref-ast?
  (lambda (ast)
    (cases ast-datatype ast
    [new-ref-ast (val) #t]
    [else #f])))

;; new-ref-ast-val :: ast? -> ast?
(define new-ref-ast-val
  (lambda (ast)
    (cases ast-datatype ast
    [new-ref-ast (val) val]
    [else (error "Incompatible types")])))

;; deref-ast? :: ast? -> boolean?
(define deref-ast?
  (lambda (ast)
    (cases ast-datatype ast
    [deref-ast (var) #t]
    [else #f])))

;; deref-ast-var :: ast? -> ast?
(define deref-ast-var
  (lambda (ast)
    (cases ast-datatype ast
    [deref-ast (var) var]
    [else (error "Incompatible types")])))

;; setref-ast? :: ast? -> boolean?
(define setref-ast?
  (lambda (ast)
    (cases ast-datatype ast
    [setref-ast (var val) #t]
    [else #f])))

;; setref-ast-var :: ast? -> ast?
(define setref-ast-var
  (lambda (ast)
    (cases ast-datatype ast
    [setref-ast (var val) var]
    [else (error "Incompatible types")])))

;; setref-ast-val :: ast? -> ast?
(define setref-ast-val
  (lambda (ast)
    (cases ast-datatype ast
    [setref-ast (var val) val]
    [else (error "Incompatible types")])))

;; seq-ast? :: ast? -> boolean?
(define seq-ast?
  (lambda (ast)
    (cases ast-datatype ast
    [seq-ast (body) #t]
    [else #f])))

;; seq-ast-body :: ast? -> ast?
(define seq-ast-body
  (lambda (ast)
    (cases ast-datatype ast
    [seq-ast (body) body]
    [else (error "Incompatible types")])))
