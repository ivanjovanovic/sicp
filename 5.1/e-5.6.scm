; Exercise 5.6.  Ben Bitdiddle observes that the Fibonacci machine's
; controller sequence has an extra save and an extra restore, which can
; be removed to make a faster machine. Where are these instructions?
; ------------------------------------------------------------

; This exercise introduces a notion of the possible optimizations in the
; machine code. It often happens that is possible to reorganize, remove
; or optimize in a different way set of instructions and not harm the
; code executed but optimize its efficiency.
; In this particular case, it is safe to remove pair of instructions
; that just put and restore something from stack without being used in
; the meantime. 
