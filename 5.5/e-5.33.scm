; Exercise 5.33.
;
; Consider the following definition of a factorial
; procedure, which is slightly different from the one given above:

(define (factorial-alt n)
  (if (= n 1)
      1
      (* n (factorial-alt (- n 1)))))

; Compile this procedure and compare the resulting code with that
; produced for factorial. Explain any differences you find. Does either
; program execute more efficiently than the other?
; ------------------------------------------------------------

(load "5.5.scm")

(define object-code
  (compile
    '(define (factorial-alt n)
        (if (= n 1)
        1
        (* n (factorial-alt (- n 1)))))
    'val
    'next))

(format-object-code (instruction-list object-code))


; Here I have generated formatted compilation result in order to compare it with the diff tool.

; First a bit of clarification. The order in which argument evaluation comes in the object code
; is in the inverse order (right-to-left) because our compiler is building the argl for the proc to be applied
; by consing evaluated arguments (cons arg argl). Therefore in order to have proper list of arguments
; we have to evaluate them inverselly. This can be seen as well here in the two examples we are comparing.
;
; In first example 5.5-factorial-compiled.scm we see that first we fetch value of n and then evaluate
; the recursive call. In order to do this we have to save argument list of the previous application so
; when we come back we have tproper state to continue our application from.
;
; In oder case, we first jump into the evaluation of recursive call and there we have no need to
; preserve the current state of argument list since we didn't build one yet. Therefore this implementation
; will be faster for one pair of stack operations for every recursive calls.
;
; Message of this exercise is that it matters to know the internals of the machine you are working with
; if fine tunings are to be made.
