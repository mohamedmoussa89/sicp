#lang sicp

; (a, b) -> 2^a 3^b -> (2*2*...*2) * (3*3*...*3)
(define (pair a b)
  (* (expt 2 a) (expt 3 b)))

(define (fst p)
  (define (inner a p)
    (if (not (= (remainder p 2) 0))
        a
        (inner (+ a 1) (/ p 2))))
  (inner 0 p))

(define (snd p)
  (define (inner b p)
    (if (not (= (remainder p 3) 0))
        b
        (inner (+ b 1) (/ p 3))))
  (inner 0 p))

(fst (pair 4 8))
(snd (pair 4 8))