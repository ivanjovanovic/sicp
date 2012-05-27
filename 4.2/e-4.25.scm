; Exercise 4.25.  Suppose that (in ordinary applicative-order Scheme) we define
; unless as shown above and then define factorial in terms of unless as

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

; What happens if we attempt to evaluate (factorial 5)? Will our definitions
; work in a normal-order language?
; ------------------------------------------------------------

; Applicative-order evaluation means that all the operands are evaluated before
; operator is applied on their values.
; In our case that means that second argument evaluation would trigger the infinite recursion
; and will never come to appllying unless on the operands.
;
; In normal order evaluation, only the condition will be evaluated and consequent
; will be evaluated only after the condition turned out false, when it is true (= n 1) => #t
; it will finish the recursion.
