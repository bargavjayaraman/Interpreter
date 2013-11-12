#lang racket

(require eopl/eopl)
(require "env.ss")

(provide
 expressible-value?
 denotable-value?)
 
 
(define expressible-value?
  (or/c number? boolean? proc?))
 
 
(define denotable-value?
  (or/c number? boolean? proc?))
