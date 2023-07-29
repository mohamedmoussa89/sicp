#lang sicp

(define (cube x) (* x x x))

; sum f(i) for i in [a, b) 
(define (sum f a next b)
  (if (>= a b)
      0
      (+ (f a) (sum f (next a) next b))))

; simpson rule to estimate integral of function
;   h/3 ( y_0 + 4y_1 + 2y_2 + ... 2y_(n-2) + 4y_(n-1) + y_n)
; = h/3 ( y_0 + y_n + 4*(y_1 + y_3 + ... ) + 2*(y_2 + y_4 + ...) )
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (add-2 x) (+ x 2))
  (* (/ h 3.0)
     (+ (y 0)
        (y n)
        (* 4 (sum y 1 add-2 n))
        (* 2 (sum y 2 add-2 n)))))

(simpson cube 0 1 2)