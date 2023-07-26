#lang sicp

; recursive version of b^n
(define (exp-fast-r b n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp-fast-r b (/ n 2))))
        (else (* b (exp-fast-r b (- n 1))))))

; iterative version of b^n
(define (exp-fast-i b n)
  (define (exp-iter a bb m)
    (if (= m 0)
        a
        (if (even? m)
            (exp-iter a (square bb) (/ m 2))
            (exp-iter (* a bb) bb (- m 1)))))
  (exp-iter 1 b n))

(define (even? x) (= (remainder x 2) 0))

(define (square x) (* x x))

(exp-fast-i 2 101)
(exp-fast-r 2 101)
