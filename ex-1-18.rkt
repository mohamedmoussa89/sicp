#lang sicp

(define (even? x) (= (remainder x 2) 0))
(define (halve x) (/ x 2))
(define (double x) (* x 2))

; iterative multiply with log(b) runtime 
(define (mult a b)
  (define (mult-iter s aa bb)
    (cond ((= bb 0) s)
          ((even? bb) (mult-iter s (double aa) (halve bb)))
          (else (mult-iter (+ s aa) aa (- bb 1)))))
  (mult-iter 0 a b))

(mult 123 1)
(mult 123 10)
(mult 123 100)
(mult 123 1000)
(mult 123 10000)