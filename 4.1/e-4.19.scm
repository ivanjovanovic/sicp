; Exercise 4.19.  Ben Bitdiddle, Alyssa P. Hacker, and
; Eva Lu Ator are arguing about the desired result of
; evaluating the expression

(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

; Ben asserts that the result should be obtained using
; the sequential rule for define: b is defined to be 11,
; then a is defined to be 5, so the result is 16. Alyssa
; objects that mutual recursion requires the simultaneous
; scope rule for internal procedure definitions, and that
; it is unreasonable to treat procedure names differently
; from other names. Thus, she argues for the mechanism
; implemented in exercise 4.16. This would lead to a
; being unassigned at the time that the value for b is to
; be computed. Hence, in Alyssa's view the procedure
; should produce an error. Eva has a third opinion. She
; says that if the definitions of a and b are truly meant
; to be simultaneous, then the value 5 for a should be
; used in evaluating b. Hence, in Eva's view a should be
; 5, b should be 15, and the result should be 20. Which
; (if any) of these viewpoints do you support? Can you
; devise a way to implement internal definitions so that
; they behave as Eva prefers?
; ------------------------------------------------------------


; I would say that in order to keep the predictability of
; the computational model we use, and remove possibilities for ambiguous
; interpretations, throwing an error is good option, as authors suggest.
;
; To implement Evas point of view, I guess it would need one more convention
; additional to the `sequental` scoping.
; If there is a direct value definition as (define a 5), then variable
; should be initialized to that value and not unassigned, before all other definitions.
; That way using it in any other definition is straightforward.
