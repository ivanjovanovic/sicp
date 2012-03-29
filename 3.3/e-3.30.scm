; Exercise 3.30.
;
; Figure 3.27 shows a ripple-carry adder formed by
; stringing together n full-adders. This is the simplest form of
; parallel adder for adding two n-bit binary numbers. The inputs
; A1, A2, A3, ..., An and B1, B2, B3, ..., Bn are the two binary
; numbers to be added (each Ak and Bk is a 0 or a 1). The circuit
; generates S1, S2, S3, ..., Sn, the n bits of the sum, and C, the
; carry from the addition. Write a procedure ripple-carry-adder
; that generates this circuit. The procedure should take as
; arguments three lists of n wires each -- the Ak, the Bk, and the
; Sk -- and also another wire C. The major drawback of the
; ripple-carry adder is the need to wait for the carry signals to
; propagate. What is the delay needed to obtain the complete
; output from an n-bit ripple-carry adder, expressed in terms of
; the delays for and-gates, or-gates, and inverters?
;
; Ripple carry adder:
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_fig_3.27

(define (ripple-adder a-in b-in s-out c-in)
  (let ((cn (make-wire)))
    (if (null? (cdr a-in))
      ; this will set signal to zero in the last recursive call
      (set-signal! cn 0)

      ; we go here first building the call stack by going the deepest we can in the recursion
      (ripple-adder (cdr a-in) (cdr b-in) (cdr s-out) cn))

    ; this one is done on the rollback from recursion with appropriatelly set local variables on the stack
    (full-adder (car a-in) (car b-in) c-in (car s-out) cn)))

; ENLIGHTENMENT:
; Here we see one way of doing recursion that we didn't do much in fact.
; We do recursive calls on the top of recursive method and then only after we
; write the function that will actually be executed.
; (if (null? (cdr a-in))) defines the recursion termination condition where procedure
; starts to evaluate. We can see this as a way to do recursion in two separate steps
; 1. Create recursivelly environments that are needed for all the iterations
; 2. Evaluate the body or the procedure in all environments by rolling back the recursion.

; To see what the delay is, in principle, we have to define full-adder in the terms of basic components and
; see what is the delay of one full-adder. Since we have N of them, we have to multiply the
; overall delay with N. More precise and detailed explanation of delay and structure of adders can be found
; on Wikipedia: http://en.wikipedia.org/wiki/Adder_(electronics)
