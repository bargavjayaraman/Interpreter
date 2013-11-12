#lang racket
(require eopl)
(require "ast.ss")

(provide env?)
(provide env)
(provide lookup-env)
(provide init-env)

(provide proc)
(provide proc?)

(provide primitive)
(provide primitive?)
(provide primitive-op)
(provide primitive-sig)

(provide closure)
(provide closure?)
(provide closure-formals)
(provide closure-body)
(provide closure-env)

(provide empty-env)
(provide extended-env)

(provide extended-fix-env)
(provide extended-fix-env?)
(provide extended-fix-env-fids)
(provide extended-fix-env-fformals)
(provide extended-fix-env-fbodies)
(provide extended-fix-env-outer-env)


;; Datatype definitions
(define-datatype env env?
  [empty-env]
  [extended-env (syms (list-of symbol?)) (vals (list-of any/c)) (outer-env env?)]
  [extended-fix-env (f-ids (list-of symbol?)) (f-formals (list-of (list-of symbol?))) (f-bodies (list-of ast?)) (outer-env env?)])

;; Datatype defintions
(define-datatype proc proc?
  [primitive (op procedure?) (sig (list-of procedure?))]
  [closure (formals (list-of symbol?)) (body ast?) (env env?)])

;; empty-env? :: env? -> boolean?
(define empty-env? 
  (lambda (e)
    (cases env e
      [empty-env () #t]
      [else #f])))

;; extended-env? :: env? -> boolean?
(define extended-env?
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) #t]
      [else #f])))

;; extended-env-ids :: env? -> list-of symbol?
(define extended-env-ids
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) syms]
      [else (error "Incompatible types")])))

;; extended-env-vals :: env? -> list-of any/c?
(define extended-env-vals
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) vals]
      [else (error "Incompatible types")])))

;; extended-env-outer-env :: env? -> env?
(define extended-env-outer-env
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) outer-env]
      [else (error "Incompatible types")])))

;;lookup-env : env? symbol? -> any/c (ie any scheme value)
 (define lookup-env
   (lambda (e x)
    (cases env e 
      [empty-env() (error "Not found in environment")]
      [extended-env (syms vals outer-env)
         (let ([i (list-index syms x)])
               (cond 
                 [( = i -1) (lookup-env outer-env x)]
                 [#t (list-ref vals i)]))]
      [extended-fix-env (f-ids f-formals f-bodies outer-env)
        (let ([j (list-index f-ids x)])
             (cond
                  [(= j -1) (lookup-env outer-env x)]
                  [#t (let ([formals (list-ref f-formals j)]
                         [body (list-ref f-bodies j)])
                         (closure formals body e))]))]))) 

;; HELPER FUNCTION FOR EXTENDED-ENV
(define list-index
  (lambda(ls x)
    (letrec ([loop
              (lambda (ls ans)
                (cond
                  [(null? ls) -1]
                  [(eq? (first ls) x) ans]
                  [#t (loop (rest ls) (+ 1 ans))]))])
      (loop ls 0))))

;; apply-app-sign :: symbol? -> primitive?
(define apply-app-sign
  (lambda(rator) 
    (cond 
      [(eq? rator '+) (primitive + (list number? num-checker?))]
      [(eq? rator '-) (primitive - (list number? num-checker?))]
      [(eq? rator '*) (primitive * (list number? num-checker?))]
      [(eq? rator '/) (primitive / (list number? num-checker?))]
      [(eq? rator '<) (primitive < (list boolean? num-checker?))]
      [(eq? rator '>) (primitive > (list boolean? num-checker?))]
      [(eq? rator '<=) (primitive <= (list boolean? num-checker?))]
      [(eq? rator '>=) (primitive >= (list boolean? num-checker?))]
      [(eq? rator '=) (primitive = (list boolean? num-checker?))]
      [(eq? rator 'add1) (primitive add1 (list number? addsub1-checker?))]
      [(eq? rator 'sub1) (primitive sub1 (list number? addsub1-checker?))]
      [(eq? rator 'not) (primitive not (list boolean? bool-checker?))]
      [(eq? rator '0?) (primitive zero? (list boolean? addsub1-checker?))]
      )))

;; num-checker? :: list? -> boolean?
(define num-checker?
  (lambda (x)
    (cond
      [(list? x) (andmap number? x)]
      [#t #f])))

;; bool-checker? :: list? -> boolean?
(define bool-checker?
  (lambda (x)
    (cond
      [(and (list? x)(= 1 (length x))) (andmap boolean? x)]
      [#t #f])))

;; addsub1-checker? :: list? -> boolean?
(define addsub1-checker?
  (lambda (x)
    (cond
      [(and (list? x) (= 1 (length x))) (andmap number? x)]
      [#t #f])))

;; Operators in the interpreter
(define ops '(+ - * / < > <= >= = sub1 add1 0? not))

;; Initial environment
(define init-env (extended-env ops (map apply-app-sign ops) (empty-env) ))

;;primitive? : proc? -> boolean?
(define primitive?
  (lambda (p)
    (cases proc p
      [primitive (op sig) #t]
      [else #f])))

;;primitive-op : proc? -> procedure?
(define primitive-op
  (lambda (p)
    (cases proc p
      [primitive (op sig) op]
      [else (error "Incompatible types")])))

;;primitive-sig : proc? -> list-of procedure?
(define primitive-sig
  (lambda (p)
    (cases proc p
      [primitive (op sig) sig]
      [else (error "Incompatible types")])))

;;closure? : proc? -> boolean?
(define closure?
  (lambda (c)
    (cases proc c
      [closure (formals body env) #t]
      [else #f])))

;;closure-formal : proc? -> list-of symbols?
(define closure-formals
  (lambda (c)
    (cases proc c
      [closure (formals body env) formals]
      [else (error "Incomapatible types")])))

;;closure-body : proc? -> ast?
(define closure-body
  (lambda (c)
    (cases proc c
      [closure (formals body env) body]
      [else (error "Incompatible types")])))

;; closure-env : proc? -> env?
(define closure-env
  (lambda (c)
    (cases proc c
      [closure (formals body env) env]
      [else (error "Incompatible types")])))

;; extended-fix-env? :: env? -> boolean?
(define extended-fix-env?
  (lambda (ast)
    (cases env ast
      [extended-fix-env (f-ids f-formals f-bodies outer-env) #t]
      [else #f])))

;; extended-fix-env-fids :: extended-fix-env? -> list-of symbol?
(define extended-fix-env-fids
  (lambda (ast)
    (cases env ast
      [extended-fix-env (f-ids f-formals f-bodies outer-env) f-ids]
      [else (error "Incompatible types")])))

;; extended-fix-env-fformals :: extended-fix-env? -> list-of (list-of symbol?)
(define extended-fix-env-fformals
  (lambda (ast)
    (cases env ast
      [extended-fix-env (f-ids f-formals f-bodies outer-env) f-formals]
      [else (error "Incompatible types")])))

;; extended-fix-env-fbodies :: extended-fix-env? -> list-of ast?
(define extended-fix-env-fbodies
  (lambda (ast)
    (cases env ast
      [extended-fix-env (f-ids f-formals f-bodies outer-env) f-bodies]
      [else (error "Incompatible types")])))

;; extended-fix-env-fids :: extended-fix-env? -> env?
(define extended-fix-env-outer-env
  (lambda (ast)
    (cases env ast
      [extended-fix-env (f-ids f-formals f-bodies outer-env) outer-env]
      [else (error "Incompatible types")])))

