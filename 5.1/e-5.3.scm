; Exercise 5.3.  Design a machine to compute square roots using Newton's
; method, as described in section 1.1.7:

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; Begin by assuming that good-enough? and improve operations are
; available as primitives. Then show how to expand these in terms of
; arithmetic operations. Describe each version of the sqrt machine
; design by drawing a data-path diagram and writing a controller
; definition in the register-machine language.
; ------------------------------------------------------------

; I will just write the machine-language version without drawings.

; a) With primitive operations

(controller
loop-guess
  (test (op good-enough?) (reg guess))
  (branch (label guessing-done))
  (assign guess (op improve) (reg guess))
  (goto (label loop-guess))
guessing-done)

; Problem I see here is that good-enough? and improve do not by default
; assign value to some register, so we'll need to add additional one for
; these to exchange data with the other parts of the code? I'll name it
; t

; Since good-enough? and improve are not primitives now, we'll have to
; take abs, square and average as primitves. Unwinding these will lead
; us to a deeper level of this machine ...

(controller
init
  (assign x (const 1))
loop-guess
  (assign t (op square) (reg guess))
  (assign t (op -) (reg t) (reg x))
  (assign t (op abs) (reg t))
  (test (op <) (reg t) (const 0.001))
  (branch (label guessing-done))
  (assign t (op /) (reg x) (reg guess))
  (assign guess (op average) (reg guess) (reg t))
  (goto (label loop-guess))
guessing-done)
