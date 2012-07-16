; specfication of the GCD machine in the specialized language for
; describing the register machine

(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))

 (operations
  ((name rem)
   (inputs (register a) (register b)))
  ((name =)
   (inputs (register b) (constant 0)))))

(controller
 test-b                           ; label
   (test =)                       ; test
   (branch (label gcd-done))      ; conditional branch
   (t<-r)                         ; button push
   (a<-b)                         ; button push
   (b<-t)                         ; button push
   (goto (label test-b))          ; unconditional branch
 gcd-done)                        ; label

; If we look above it is pretty complicated to literally describe
; complex machines like this. Therefore we can introduce a bit more
; abstracted version of this machine language.

(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)


; Easiest way to introduce need to save state between the procedure
; calls is by defining the machine for factorial computation. We use
; stack to save and restore intermediate states.

(controller
   (assign continue (label fact-done))     ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1! = 1
   (goto (reg continue))                   ; return to caller
 fact-done)


