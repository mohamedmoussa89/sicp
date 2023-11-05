#lang sicp

; Compute f_i = N_i / (D_i + f_i+1), terminating at f_k = 1.0

; Recursive version
(define (cont-frac K)
  (define (iter k)
    (if (= k K)
        1.0
        (/ 1.0 (+ 1.0 (iter (+ k 1))))))
  (iter 1))

; Iterative Version
(define (cont-frac-2 K)
  (define (iter result k)
    (if (= k 1)
        result
        (iter (/ 1.0 (+ 1.0 result)) (- k 1))))
  (iter 1.0 K))
    

; Experiment, evaluate as fixed point of function y = 1 / (1 + y)
(define (fixed-point f guess)
  (let ((fg (f guess)))
    (if (close-enough? guess fg)
        fg
        (fixed-point f fg))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.0001))


(cont-frac 11)
(cont-frac-2 11)
(fixed-point (lambda (x) (/ 1.0 (+ 1 x))) 1.0)

(define phi 1.618033988749894)
(/ 1 phi)
        