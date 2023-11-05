#lang sicp

(define (unit x) x)

(define (square x) (* x x))

(define zero
  (lambda (f) (lambda (x) x)))

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add n m)
  (lambda (f) (lambda (x) ((n f) ((m f) x)))))
        

((one square) 2)
((two square) 2)
(define three (add one two))
((three square) 2)