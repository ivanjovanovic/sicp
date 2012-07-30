; Exercise 5.36.  What order of evaluation does our compiler produce for
; operands of a combination? Is it left-to-right, right-to-left, or some other
; order? Where in the compiler is this order determined? Modify the compiler so
; that it produces some other order of evaluation. (See the discussion of order
; of evaluation for the explicit-control evaluator in section 5.4.1.) How does
; changing the order of operand evaluation affect the efficiency of the code
; that constructs the argument list?
; ------------------------------------------------------------

; Evaluation is done from right to left, because the way we use argument list is that we
; cons on top of it the arguments so in order to preserve correct order we do it backwards.
; It is defined in couple of places. First in way machine uses it, and second in the place where we
; generate code for computation of the arguments.
;
; I guess efficiency depends on the way we write code, as we saw it could be either way.
