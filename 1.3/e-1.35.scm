; Exercise 1.35.
;
; Show that the golden ratio Phi (section 1.2.2) is a fixed point of the
; transformation x ->  1 + 1/x, and use this fact to compute by means of the fixed-point procedure.
;
; ---------------------------

(load "1.3.scm") ; reusing fixed-point

(define (phi precision)
  (fixed-point
    (lambda (x) (+ 1 (/ 1 x)))
    1.0
    precision))

(display (phi 0.00000001))
(newline)
