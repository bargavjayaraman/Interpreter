#lang racket
(require eopl)
(require "semantic-domain.ss")

(provide store)
;(provide ref)
(provide ref?)
(provide new-ref)
(provide deref)
(provide setref)

(define store (list-of denotable-value?))

;(define ref (number?))

(define ref?
  (lambda (s)
    (lambda (i)
      (< i (length s)))))

;;new-ref? : store? storable-value? -> new-store? ref?
(define new-ref
   (lambda (s v)
     (list (cons v s) (length s))))

;;deref? : store? ref? -> storable-value?
(define deref
  (lambda (s r)
   (let ([n (length s)])
    (cond 
          [(< r n) (list-ref s (- n r 1))]
          [#t (error "out of range")]))))

;;setref? : store? ref? storable-value? -> new-store?
(define setref
  (lambda (s r v)
    (let ([n (length s)])
    (cond 
          [(< r n)
            (replace s (- n r 1) v)]
          [#t (error "out of range")]))))

;;replace : list? number? number? -> list?
(define replace
  (lambda(ls x y)
    (append (reverse (list-tail (reverse ls) (- (length ls) x))) (list y) (list-tail ls (+ x 1)))))

