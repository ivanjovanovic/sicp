; Exercise 5.11.  When we introduced save and restore in section 5.1.4, we
; didn't specify what would happen if you tried to restore a register that
; was not the last one saved, as in the sequence

; (save y)
; (save x)
; (restore y)

; There are several reasonable possibilities for the meaning of restore:

; a.  (restore y) puts into y the last value saved on the stack, regardless
; of what register that value came from. This is the way our simulator
; behaves. Show how to take advantage of this behavior to eliminate one
; instruction from the Fibonacci machine of section 5.1.4 (figure 5.12).

; b.  (restore y) puts into y the last value saved on the stack, but only if
; that value was saved from y; otherwise, it signals an error. Modify the
; simulator to behave this way. You will have to change save to put the
; register name on the stack along with the value.

; c.  (restore y) puts into y the last value saved from y regardless of what
; other registers were saved after y and not restored. Modify the simulator
; to behave this way. You will have to associate a separate stack with each
; register. You should make the initialize-stack operation initialize all the
; register stacks.
;
; ------------------------------------------------------------

; a. Sequence of the instructions marked with the label `afterfib-n-1` contains
;    (restore continu) and afterwards (save continue) instructions. Meanwhile, between these
;    two instructions, there is no manipulation of this register so in fact it would just take
;    it from stack and return it there.
;    Since, next instruction is to assign something to the `continue` register, then we can just remove
;    two aforementioned instructions that in fact do not have any effect in this constalation.
;
; b. here, we have to upgrade the make-save and make-restore procedures to save the name of the register
;    together with the value and to check on name match on restoring.

; save to stack execution procedure
(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (cons reg-name (get-contents reg))) ; pushing reg name with the valu
      (advance-pc pc))))

; restore from stack execution procedure
(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (let* ((stack-val (pop stack))
             (stack-reg-name (car stack-val)))
        (if (not (eq? stack-reg-name reg-name)) ; check if it is same
          (error "Restoring value from stack into the wrong register -- RESTORE" reg-name stack-reg-name)
          (begin
            (set-contents! reg stack-val)
            (advance-pc pc)))))))

; c. Got the point, in constraint with the time I will skip this exercise, but we would have to make the list
;    of stacks where for every register name we'll have separate stack. Then we could pull the stack out for the register name and do
;    the save and restore as usual.


