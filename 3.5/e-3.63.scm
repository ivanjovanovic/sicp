; Exercise 3.63.
;
; Louis Reasoner asks why the sqrt-stream procedure was
; not written in the following more straightforward way, without the
; local variable guesses:

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

; Alyssa P. Hacker replies that this version of the procedure is
; considerably less efficient because it performs redundant computation.
; Explain Alyssa's answer. Would the two versions still differ in
; efficiency if our implementation of delay used only (lambda () <exp>)
; without using the optimization provided by memo-proc (section 3.5.1)?
; ------------------------------------------------------------

; In this approach, every call to (sqrt-stream x) will produce new
; stream.
;
; In the example with local variable, we are building stream once,
; passing it to variable and then reusing this variable when accessing
; the elements of the stream.
;
; I guess it would be similar inefficiency with the non-memo
; implementation of delay.
