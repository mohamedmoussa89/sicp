#lang sicp

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f guess)
  (let ((fg (f guess)))
    (if (close-enough? guess fg)
        fg
        (fixed-point f fg))))

(define (fixed-point-avg f guess)
  (fixed-point (lambda (x) (average x (f x))) guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
(fixed-point-avg (lambda (x) (/ (log 1000) (log x))) 1.1)





