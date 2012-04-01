; Exercise 3.33.
;
; Using primitive multiplier, adder, and constant
; constraints, define a procedure averager that takes three connectors
; a, b, and c as inputs and establishes the constraint that the value of
; c is the average of the values of a and b.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "constraints.scm")

(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)
(set-value! a 5 'ivan)
(set-value! b 70 'ivan)

; as expected this outputs 37.5 as average for 70 and 5
(output (get-value c))
