#lang sicp

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average a b) (/ (+ a b) 2))

(define (fixed-point f guess)
  (let ((fg (f guess)))
    (if (close-enough? guess fg)
        fg
        (fixed-point f fg))))

(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

golden-ratio


