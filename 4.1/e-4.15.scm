; Exercise 4.15.
;
; Given a one-argument procedure p and an object a, p is said
; to ``halt'' on a if evaluating the expression (p a) returns a value (as
; opposed to terminating with an error message or running forever). Show that
; it is impossible to write a procedure halts? that correctly determines
; whether p halts on a for any procedure p and object a. Use the following
; reasoning: If you had such a procedure halts?, you could implement the
; following program:

;(define (run-forever) (run-forever))

;(define (try p)
  ;(if (halts? p p)
      ;(run-forever)
      ;'halted))

; Now consider evaluating the expression (try try) and show that any possible
; outcome (either halting or running forever) violates the intended behavior of
; halts?.
; ------------------------------------------------------------

; This exercise is pointing to one very fundamental topic in the theory of computation.
; So called [Halting problem](https://en.wikipedia.org/wiki/Halting_problem).
; Shortly, the problem is defined as a problem: given a program and an input, can we decide
; if program will halt (end execution and eventually return result) or it will run forever.
;
; Alan Turing proved that this kind of procedure can not be implemented. He defined the
; famous Turing machine as most general computational model in order to mathematically define
; properties of a computation and computer program and to prove the undecidability of this problem.

; Lets evaluate (try try) to see what we get.
;
; So, point is that if try doesn't halt, then during evaluation of try (halts? try try) it will start running forewer.
; In case when (halts? try try) returns true, then it will start running forever after it returns from evaluating if try halts.
