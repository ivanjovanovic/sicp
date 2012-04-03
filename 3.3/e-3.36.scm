; Exercise 3.36.
;
; Suppose we evaluate the following sequence of
; expressions in the global environment:

; (define a (make-connector))
; (define b (make-connector))
; (set-value! a 10 'user)

; At some time during evaluation of the set-value!, the following
; expression from the connector's local procedure is evaluated:

; (for-each-except setter inform-about-value constraints)

; Draw an environment diagram showing the environment in which the above
; expression is evaluated.
; ------------------------------------------------------------

; complex drawing on paper.
;
; First we have to draw set of definitions in the global space.
;
; Second we have to make connectors which create two execution environments
; that hold local state and procedures which are loal to that execution environment
;
; Third, we create the call stack for the set-value! procedure executed in global environment
; and then can see how procedures change state of the existing execution environments, how they
; relate to them and in which environment (for-each-except) is executed and how its execution environment
; looks like.
;
;
;
;
; Sidenote:
; I hope to buy scanner these days to scan some hand drawn environment diagrams, but my expectations
; are fairly low since my drawing skills are approximating zero.
