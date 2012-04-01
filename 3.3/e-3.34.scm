; Exercise 3.34.
;
; Louis Reasoner wants to build a squarer, a constraint
; device with two terminals such that the value of connector b on the
; second terminal will always be the square of the value a on the first
; terminal. He proposes the following simple device made from a
; multiplier:

; (define (squarer a b)
;   (multiplier a a b))

; There is a serious flaw in this idea. Explain.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "constraints.scm")

(define (squarer a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))
(squarer a b)

; Here is easy to see that there is a problem with this constraint.
; Because of the way multiplier is implemented, we can not set the
; square value and get the value of squared quantities. It works only in
; the direction of setting the quantity to be squared and then reading
; the result
(set-value! b 100 'ivan)
(output (get-value b))
(output (get-value a)) ; outputs #f which is wrong
