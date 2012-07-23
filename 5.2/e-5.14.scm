; Exercise 5.14.
;
; Measure the number of pushes and the maximum stack
; depth required to compute n! for various small values of n using the
; factorial machine shown in figure 5.11. From your data determine
; formulas in terms of n for the total number of push operations and the
; maximum stack depth used in computing n! for any n > 1. Note that each
; of these is a linear function of n and is thus determined by two
; constants. In order to get the statistics printed, you will have to
; augment the factorial machine with instructions to initialize the
; stack and print the statistics. You may want to also modify the
; machine so that it repeatedly reads a value for n, computes the
; factorial, and prints the result (as we did for the GCD machine in
; figure 5.4), so that you will not have to repeatedly invoke
; get-register-contents, set-register-contents!, and start.
; ------------------------------------------------------------

(load "5.2.4.scm")
(load "../helpers.scm")

(define fact-machine
  (make-machine
    '(continue n val)
    ; adding the read operation that reads frm STDIN
    (list (list '= =) (list '- -) (list '* *) (list 'read read))
    '(init
       (perform (op initialize-stack))
       (assign n (op read))
       (assign continue (label fact-done))
      fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
      after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
      base-case
       (assign val (const 1))
       (goto (reg continue))
      fact-done
       (perform (op print-stack-statistics)))))

; REPL

(define (repl)
  (start fact-machine)
  (repl))

(repl)

; 10 -> 18
; 100 -> 198
; 200 -> 398
; n -> 2n - 2
