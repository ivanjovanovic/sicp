; Exercise 5.13.  Modify the simulator so that it uses the controller
; sequence to determine what registers the machine has rather than
; requiring a list of registers as an argument to make-machine. Instead
; of pre-allocating the registers in make-machine, you can allocate them
; one at a time when they are first seen during assembly of the
; instructions.
; ------------------------------------------------------------

; This is not so difficult task.
;
; First hint is to update get-register-content and set-register-content
; to create the register if there is no one already.

