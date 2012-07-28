; Exercise 5.24.
;
; Implement cond as a new basic special form without
; reducing it to if. You will have to construct a loop that tests the
; predicates of successive cond clauses until you find one that is true,
; and then use ev-sequence to evaluate the actions of the clause.
; ------------------------------------------------------------

; in this case we do not use already given reduction function so we do
; not need to add new primitive operations to the machine as in previous
; exercise.

; We will, though, upgrade the eval-dispatch to jump to the condition
; handling label when one is detected as in previous example.
eval-dispatch
  ...
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  ...

; we consider that we have all the functions that are making abstraction around the
; representation of the conditional syntax available in the register machine as primitive operations.
; They are implemented in 4.1.scm where we impemented the metacircular evaluator.


; our condition handling code will be much more extensive this time.
; Since I wanted to understand exactly why every element is there I tried
; to comment as precise most of the lines.
ev-cond-init
  ; keep the list of conditional clauses
  (assign unev (op cond-clauses) (reg exp))
; starting point for the loop we will return when we need to evaluate the next clause.
ev-cond-clause-evaluator-loop
  ; assigning the expression to be evaluated
  (assign exp (op first-exp) (reg unev))
  ; check if it is the else clause
  (test (op cond-else-clause?) (reg exp))
  ; if so jump to the evaluation of the final expression
  (branch (label ev-cond-evaluate-sequence))
  ; prepare for asking for the evaluation of the test predicate
  ; which means going through the evaluation process. After the evaluation process
  ; the result will be in val. We have to define where to go after the evaluation is done.
  ; But before we have to save the state of the machine

  ; Evaluation process might need to call subroutine and change continue, so we have to know
  ; how to go back from where we came, therefor we save continue.
  (save continue)
  (save env)
  (save unev)
  (save exp)
  (assign exp (op cond-predicate) (reg exp))
  (assign continue (label ev-cond-decide-after-predicate-evaluation))
  (goto (label eval-dispatch))

; after the predicate is evaluated we want to see where we go
ev-cond-decide-after-predicate-evaluation
  ; get back the context of the execution of cond
  (restore continue)
  (restore env)
  (restore unev)
  (restore exp) ; exp now contains the whole clause that was saved above
  ; test what did evaluation process produce
  (test (op true?) (reg val))
  ; if true, execute the sequence matching this predicate
  (branch (label ev-cond-evaluate-sequence))
  ; otherwise go back through the loop, just meanwhile, update the
  ; registers to contain proper values
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-cond-clause-evaluator-loop))

; here we are when we need to execute the actual sequence.
; ev-sequence expects the sequence in the register unev
ev-cond-evaluate-sequence
  (assign unev (op cond-actions) (reg exp))
  ; since we are calling the subroutine and we would like after that
  ; to move from where we came from, because we are leaving the condition procedure
  ; we save continue on the stack so unrollig the stack will pick it up.
  (save continue)
  (goto (label ev-sequence))
