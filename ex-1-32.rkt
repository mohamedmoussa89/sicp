#lang sicp

(define (accumulate combiner null-value)
  (define (f term a next b)
    (if (>= a b)
        null-value
        (combiner (term a) (f term (next a) next b))))
  f)

(define (accumulate-i combiner null-value)  
  (lambda (term a next b)
    (define (iter a result)
      (if (>= a b)
          result
          (iter (next a) (combiner (term a) result))))
    (iter a null-value)))

(define sum (accumulate-i + 0))
(define product (accumulate-i * 1))

(define (id x) x)
(define (inc x) (+ x 1))

(sum id 1 inc 5)
(+ 1 2 3 4)

(product id 1 inc 5)
(* 1 2 3 4)
