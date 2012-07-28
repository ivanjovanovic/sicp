; Exercise 5.30.  Our evaluator currently catches and signals only two kinds of
; errors -- unknown expression types and unknown procedure types. Other errors
; will take us out of the evaluator read-eval-print loop. When we run the
; evaluator using the register-machine simulator, these errors are caught by
; the underlying Scheme system. This is analogous to the computer crashing when
; a user program makes an error.32 It is a large project to make a real error
; system work, but it is well worth the effort to understand what is involved
; here.

; a. Errors that occur in the evaluation process, such as an attempt to access
; an unbound variable, could be caught by changing the lookup operation to make
; it return a distinguished condition code, which cannot be a possible value of
; any user variable. The evaluator can test for this condition code and then do
; what is necessary to go to signal-error. Find all of the places in the
; evaluator where such a change is necessary and fix them. This is lots of
; work.

; b. Much worse is the problem of handling errors that are signaled by applying
; primitive procedures, such as an attempt to divide by zero or an attempt to
; extract the car of a symbol. In a professionally written high-quality system,
; each primitive application is checked for safety as part of the primitive.
; For example, every call to car could first check that the argument is a pair.
; If the argument is not a pair, the application would return a distinguished
; condition code to the evaluator, which would then report the failure. We
; could arrange for this in our register-machine simulator by making each
; primitive procedure check for applicability and returning an appropriate
; distinguished condition code on failure. Then the primitive-apply code in the
; evaluator can check for the condition code and go to signal-error if
; necessary. Build this structure and make it work. This is a major project.
; ------------------------------------------------------------
