; Exercise 2.8.
;
; Using reasoning analogous to Alyssa's, describe how the difference of two 
; intervals may be computed. Define a corresponding subtraction procedure, called sub-interval.
; -------------------------------------

(load "2.1.scm")
(load "e-2.7.scm")

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; (display (lower-bound (sub-interval r2 r1)))
; (newline)
