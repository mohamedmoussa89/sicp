#lang sicp

(define (make-interval a b)
  (cons a b))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (contains? i v)
  (and (< (lower-bound i) v) (< v (upper-bound i))))

(define (negative? i)
  (< (upper-bound i) 0))

(define (positive? i)
  (> (lower-bound i) 0))

(define (cross-zero? i)
  (contains? i 0))
  
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval-2 x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    
    (cond ((and (positive? x) (positive? y))
           (make-interval (* lx ly) (* ux uy)))
          
          ((and (positive? x) (cross-zero? y))
           (make-interval (* ux ly) (* ux uy)))

          ((and (positive? x) (negative? y))
           (make-interval (* ux uy) (* lx ly)))

          ((and (negative? x) (cross-zero? y))
           (make-interval (* lx uy) (* lx ly)))

          ((and (negative? x) (negative? y))
           (make-interval (* ux uy) (* lx ly)))

          ((and (cross-zero? x) (cross-zero? y))
           (let ((l1 (* ux ly))
                 (l2 (* lx uy))
                 (u1 (* lx ly))
                 (u2 (* ux uy)))
             (make-interval (min l1 l2) (max u1 u2))))

          ((and (negative? x) (positive? y))
           (mul-interval y x))
          
          ((and (cross-zero? x) (positive? y))
           (mul-interval y x))

          ((and (cross-zero? x) (negative? y))
           (mul-interval y x)))))

(define (div-interval x y)
  (if (contains? y 0)
      (make-interval (/ (lower-bound x) 0.0) (/ (upper-bound x) 0.0))
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval
   x
   (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(mul-interval-2 (make-interval -1 2) (make-interval -3 4))
(mul-interval   (make-interval -1 2) (make-interval -3 4))
