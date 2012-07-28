; Exercise 5.28.
;
; Modify the definition of the evaluator by changing
; eval-sequence as described in section 5.4.2 so that the evaluator is no
; longer tail-recursive. Rerun your experiments from exercises 5.26 and 5.27
; to demonstrate that both versions of the factorial procedure now require
; space that grows linearly with their input.
; ------------------------------------------------------------
;
; For doing this exercise use ch5-eceval-non-tail-recursive.scm

; what we see here is that now even the iterative process doesn't take
; constant stack space but it grows lineraly as with the recursive process.
