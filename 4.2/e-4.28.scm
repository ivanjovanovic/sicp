; Exercise 4.28.
;
; Eval uses actual-value rather than eval to evaluate
; the operator before passing it to apply, in order to force the value
; of the operator. Give an example that demonstrates the need for this
; forcing.
; ------------------------------------------------------------

; whenever the operator is to be resolved by evaluation instead
; given directly. For example, if (deposit account) would return the
; procedure that does deposit we would need to have it evaluated before
; application and not treated as a thunk:
;
; ((deposit account) 100)
