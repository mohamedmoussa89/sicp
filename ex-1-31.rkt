#lang sicp

(define (id x) x)
(define (inc x) (+ x 1))

; recursive definition
(define (product f a next b)
  (if (>= a b)
      1
      (* (f a) (product f (next a) next b))))

; iterative definition
(define (product-i f a next b)
  (define (iter a result)
    (if (>= a b)
        result
        (iter (next a) (* (f a) result))))
  (iter a 1))

(define (factorial n)
  (product id 1 inc (+ n 1)))

; approximate pi/4 = 2/3 * 4/3 * 4/5 * 6/5 * ...
;                  = (2/3 * 4/5 * 6/7 * ...) * (4/3 * 6/5 * 8/7 * ...)
(define (approx-pi-4 n)
  (define (term-a i)
    (/ (* 2.0 i)
       (+ (* 2.0 i) 1)))
  (define (term-b i)
    (/ (* 2.0 (+ i 1))
       (- (* 2.0 (+ i 1)) 1)))
  (* (product term-a 1 inc (+ n 1))
     (product term-b 1 inc (+ n 1))))

(approx-pi-4 1)
(approx-pi-4 10)
(approx-pi-4 100)
(approx-pi-4 1000)
(approx-pi-4 10000)
(approx-pi-4 100000)