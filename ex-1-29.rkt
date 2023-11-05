#lang sicp

; get range [a, b] inclusive of start, end with step s
(define (range a b s)
  (define (iter x L)
    (if (<= x b)
        (iter (+ x s) (cons x L))
        L))
  (reverse (iter a '())))

; sums a list (tail recursive)
(define (sum L)
  (define (iter s L)
    (if (null? L)
        s
        (iter (+ s (car L)) (cdr L))))
  (iter 0 L))

; simpson rule to estimate integral of function
;   h/3 ( y_0 + 4y_1 + 2y_2 + ... 2y_(n-2) + 4y_(n-1) + y_n)
; = h/3 ( y_0 + y_n + 4*(y_1 + y_3 + ... ) + 2*(y_2 + y_4 + ...) )
;                       odds                   evens
(define (simp f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (odds y) (map y (range 1 (- n 1) 2)))
  (define (evens y) (map y (range 2 (- n 2) 2)))
  (* (/ h 3.0) (+ (y 0) (y n)
                (* 4 (sum (odds y)))
                (* 2 (sum (evens y))))))

(define (cube x) (* x x x))
(simp cube 0 1 2)
