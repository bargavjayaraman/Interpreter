#lang racket
(require eopl)
(require "ast.ss")

(provide parse)
(provide translate-ast)

;; parse :: expr? -> ast?
(define parse
  (lambda (expr)
    (cond 
      [(number? expr) (num-ast expr)]
      [(boolean? expr) (bool-ast expr)]
      [(symbol? expr) (id-ref-ast expr)]
      [(eq? (first expr) 'if) (if (eq? (length expr) 4) (if-ast (parse (second expr)) (parse (third expr)) (parse (fourth expr))) (error "Few Arguments supplied"))]
      [(eq? (first expr) 'fix) (fix-ast (map  (lambda (expr) 
                                                (let ([flist (first expr)] [slist (second expr)]) 
                                                 (fbind (first flist) (rest flist) (parse slist) ))) (second expr)) (parse (third expr)))]
      [(eq? (first expr) 'new-ref) (if (eq? (length expr) 2) (new-ref-ast (parse (second expr))) (error "Incorrect Arguments"))]
      [(eq? (first expr) 'deref) (if (eq? (length expr) 2) (deref-ast (parse (second expr))) (error "Incorrect Arguments"))]
      [(eq? (first expr) 'setref) (if (eq? (length expr) 3) (setref-ast (parse (second expr)) (parse (third expr))) (error "Incorrect Arguments"))]
      [(eq? (first expr) 'seq) (seq-ast (map parse (rest expr)))]
      [(eq? (first expr) 'and) (translate-ast (and-ast (map parse (rest expr))))]
      [(eq? (first expr) 'or) (translate-ast(or-ast (map parse (rest expr))))]
      [(eq? (first expr) 'cond) (translate-ast (cond-ast (map (lambda (expr) (clause (parse (first expr)) (parse (second expr)))) (rest expr))))]
      [(eq? (first expr) 'let) (translate-ast (let-ast (map (lambda (exp) (bind (parse (first exp)) (parse (second exp)))) (second expr)) (parse (third expr)) ))]
      [(eq? (first expr) 'function) (fn-ast (second expr) (parse (third expr)))]
      [(symbol? (first expr) ) (app-ast (parse (first expr)) (map parse (rest expr)))]
      )
    ))

;; translate-ast? :: ast? -> ast?
(define translate-ast
  (lambda (l-ast) 
    (cond
      [(and-ast? l-ast) (let ([lst (and-asts l-ast)]) 
                          (if-ast (first lst) (if (empty? (rest lst)) (bool-ast #t) (translate-ast (and-ast (rest lst)))) (bool-ast #f)))]
      [(or-ast? l-ast) (let ([lst (or-asts l-ast)]) 
                          (if-ast (first lst) (bool-ast #t) (if (empty? (rest lst)) (bool-ast #f) (translate-ast (or-ast (rest lst)))) ))]
      [(cond-ast? l-ast) (let ([lst (cond-clauses l-ast)])
                           (cond
                             [(and (id-ref-ast? (clause-test (first lst))) (eq? 'else  (id-ref-ast-id (clause-test (first lst))))) (clause-then (first lst))]
                             [#t (if-ast (clause-test (first lst)) (clause-then (first lst)) (translate-ast (cond-ast (rest lst)) ) ) ]
                              ))]
      [(let-ast? l-ast) (let ([lst (let-ast-binds l-ast)]) (app-ast (fn-ast (map id-ref-ast-id (map bind-id lst)) (let-ast-body l-ast)) (map bind-ast lst) ))]
      )))