#lang sicp

(define (cube x) (* x x x))

; sum f(i) for i in [a, b) as iteration
(define (sum f a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (+ result (f a)))))
  (iter a 0))

(define (id x) x)
(define (inc x) (+ x 1))

(sum id 1 inc 4)

