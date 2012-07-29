; Exercise 5.31.
; In evaluating a procedure application, the
; explicit-control evaluator always saves and restores the env register
; around the evaluation of the operator, saves and restores env around
; the evaluation of each operand (except the final one), saves and
; restores argl around the evaluation of each operand, and saves and
; restores proc around the evaluation of the operand sequence. For each
; of the following combinations, say which of these save and restore
; operations are superfluous and thus could be eliminated by the
; compiler's preserving mechanism:

; (f 'x 'y)

; ((f) 'x 'y)

; (f (g 'x) y)

; (f (g 'x) 'y)
;
; ------------------------------------------------------------

; I'm looking at the intruction lists of the ch5-eceval-original.scm in
; order to see which registers are changed in the process and then I
; draw conclusions. Starting from ev-application.

; 1) we do not need preservation of env since f is not to be evaluated and
; nither the arguments which are contant. Operator is a symbol in the
; environment and will be just fetched. Arguments are constants and will
; be just used as that.
;
; 2) Here I guess evaluation of the operator (f) can make change to any
; register so I would save env and argl and other registers. It
; initializes the argl though but since we are going to initialize it
; again when we do application of the result we do not care.
;
; 3) Env is safe since we do not have argument evaluation but we have to
; store argl since application of (g 'x) will pollute it. In fact we
; could optimize not to store arguments there in the list because they
; are constant and then we can as well not save argl. Before evaluation
; of y we have to store env and argl and proc since it might just change
; all of them.
;
; 4) Similar to 3  but y is here not evaluated and there is no need for
; book-keeping around it.
;
; Arg list values are just consed up so at the end they are used
; reverselly when apply-dispatch is used but they are evaluated in left
; to right order as they stand in the high-level program definition.
