#lang sicp

(define (sign x)
  (if (< x 0) -1 1))

(define
  (make-rat n d)
  (let ((g (gcd n d))
        (s (* (sign n) (sign d))))
    (cons (* s (abs (/ n g))) (abs (/ d g)))))

(define (numer r) (car r))
(define (denom r) (cdr r))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y )
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

(define (print-rat x)
  (if (= (denom x) 1)
      (display (numer x))
      (begin 
        (display (numer x))
        (display "/")
        (display (denom x)))))


(define (gcd a b)
  (define (inner a b)
    (if (= b 0)
        a
        (let ((r (remainder a b)))
          (if (= r 0)
              b
              (inner b r)))))
  (abs (if (> a b)
           (inner a b)
           (inner b a))))
