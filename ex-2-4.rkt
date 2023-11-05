#lang sicp

(define (pair x y)
  (lambda (m) (m x y)))

(define (fst z)
  (z (lambda (p q) p)))

(define (snd z)
  (z (lambda (p q) q)))

(fst (pair 1 2))
(snd (pair 5 6))
