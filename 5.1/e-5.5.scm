; Exercise 5.5.  Hand-simulate the factorial and Fibonacci machines,
; using some nontrivial input (requiring execution of at least one
; recursive call). Show the contents of the stack at each significant
; point in the execution.
; ------------------------------------------------------------

; Lets take the smallest after the trivial cases, n = 3

; start
; n = 3, val = whatever, continue = after-fact
; stack = empty
;
; after 1st loop
; n = 2, val = whatever
; stack = 3 | fact-done
;
; after 2nd loop
; n = 1, val = whatever
; stack = 2 | after-fact | 3 | fact-done
;
; after 3rd loop
; n = 1, val = 1
; stack = 2 | after-fact | 3 | fact-done
;
; jumped to after-fact
; n = 1, val = 2
; stack = 3 | fact-done
;
; jumped to after-fact
; n = 1, val = 6
; stack = empty
;
; jumped to fact-done
; n = 1, val = 6
; stack = empty
