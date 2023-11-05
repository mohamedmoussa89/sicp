#lang racket

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (sub-point a b)
  (make-point (- (x-point a) (x-point b))
              (- (y-point a) (y-point b))))

(define (min-point a b)
  (make-point (min (x-point a) (x-point b))
              (min (y-point a) (y-point b))))

(define (max-point a b)
  (make-point (max (x-point a) (x-point b))
              (max (y-point a) (y-point b))))

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

(define (make-rect a b)
  (cons (min-point a b) (max-point a b)))
  
(define rect-min car)
(define rect-max cdr)

(define (rect-diag r)
  (sub-point (rect-max r) (rect-min r)))

(define (width r)
  (x-point (rect-diag r)))
  
(define (height r)
  (y-point (rect-diag r)))

(define (area r)
  (* (width r) (height r)))

(define (perimeter r)
  (* 2 (+ (width r) (height r))))

(define r (make-rect (make-point 0 2) (make-point 5 0)))

