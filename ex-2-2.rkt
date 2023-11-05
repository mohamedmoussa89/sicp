#lang racket

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (mean a b) (* 0.5 (+ a b)))

(define (midpoint-segment s)
  (let ((a (start-segment s))
        (b (end-segment s)))    
    (make-point (mean (x-point a) (x-point b))
                (mean (y-point a) (y-point b)))))

(define (print-point p)  
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment
              (make-segment (make-point 1 2) (make-point 3 4))))