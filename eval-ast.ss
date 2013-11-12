#lang racket
(require eopl)

(require "parse.ss")
(require "ast.ss")
(require "env.ss")
(require "store.ss")
(require "semantic-domain.ss")

(provide run)
(provide eval-ast)

;; eval-ast :: ast? -> expressible-value?
(define eval-ast
  (lambda (ast env s) 
    (cases ast-datatype ast
      [num-ast (datum) datum]
      [bool-ast (datum) datum]
      [id-ref-ast (symbol) (lookup-env env symbol)]
      [if-ast (test then else) (custom-if test then else env s)]
      [fn-ast (formals body) (closure formals body env)]
      [fix-ast (fbinds body) 
               (let ([fix-ids (map (lambda (bin) (fbind-fname bin)) fbinds)]
                     [fix-formals (map (lambda (bin) (fbind-formals bin)) fbinds)]
                     [fix-bodies (map (lambda (bin) (fbind-body bin)) fbinds)]) 
                 (eval-ast body (extended-fix-env fix-ids fix-formals fix-bodies env) ) )]
      [new-ref-ast (a1)
               (let-values ([(s v) (eval-ast a1 e s)])
                 (match-let ([(list s r) (store:new-ref s v)])
                    (values s r)))]
      [deref-ast (a1)
                 (let-values ([(s v) (eval-ast a1 e s)])
                   (let ([val (store:deref s v)])
                     (values s val)))]
      [setref-ast (a1 a2)
                   (let-values ([(s1 v1) (eval-ast a1 e s)]
                                [(s2 v2) (eval-ast a2 e s)])
                     (let ([s (store:set-ref s v1 v2)])
                       (values s (void))))]
      [seq-ast (asts)
               (let-values ([(s x) (evaluate-args asts e s)])
                 (values s (list-ref x (sub1 (length x)))))][app-ast (rator rands)
        (let ([p (eval-ast rator env s)] [args (map (lambda(a) (eval-ast a env s)) rands)])
          (if (proc? p) (apply-proc p args s) (error "Rator is not proc?")))]
      [else "Error in Evaluation"]
      )))

;; custom-if :: ast? ast? ast? -> expressible-value?
(define custom-if
  (lambda (test then else env s)
    (cond
      [(eq? (eval-ast test env s) #t) (eval-ast then env s)]
      [(eq? (eval-ast test env s) #f) (eval-ast else env s)]
      [#t (error "Test expression does not evaluate to boolean")])))

(define init-store '())

;; run :: expr? -> expressible-value?
(define run 
  (lambda (expr)
    (eval-ast (parse expr) init-env init-store)))

;; apply-proc :: proc? list-of expressible-value? -> expressible-value?
(define apply-proc
   (lambda (p args s)
     (cases proc p
       [primitive (prim sig) (apply-prim-proc prim sig args)]
       [closure (formals body env) (apply-closure formals body env args s)])))

;; apply-prim-proc :: op? list-of procedure? list-of expressible-value? -> expressible-value? 
(define apply-prim-proc
    (lambda (prim sig args)
       (let ([return-type (first sig)]
             [arg-types (first (rest sig))])
         (if (arg-types args) (apply prim args) (error "Evaluation error.")))))

;; apply-closure :: closure? list-of expressible-value? -> expressible-value?
(define apply-closure
  (lambda (formals body env args s)
    (let ([new-env (extended-env formals args env)])
      (eval-ast body new-env s))))
