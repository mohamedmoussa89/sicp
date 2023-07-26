#lang sicp

(define (even? x) (= (remainder x 2) 0))
(define (halve x) (/ x 2))
(define (double x) (* x 2))

; recursive multiply with O(log(b)) time complexity
(define (mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))))

(mult 123 1)
(mult 123 10)
(mult 123 100)
(mult 123 1000)
(mult 123 10000)