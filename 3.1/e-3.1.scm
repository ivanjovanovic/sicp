; Exercise 3.1.  An accumulator is a procedure that is called repeatedly
; with a single numeric argument and accumulates its arguments into a
; sum. Each time it is called, it returns the currently accumulated sum.
; Write a procedure make-accumulator that generates accumulators, each
; maintaining an independent sum. The input to make-accumulator should
; specify the initial value of the sum; for example

; (define A (make-accumulator 5))
; (A 10) ; 15
; (A 10) ; 25
; ------------------------------------------------------------

(load "../helpers.scm")

(define (make-accumulator n)
  (let ((accumulated n)) ; initialization of environment with free variable
    (lambda (addition) ; producing the "accumulation processor" procedure
      (set! accumulated (+ accumulated addition))
      accumulated)))

; we create accumulator object that contains local state and one action
; which is returned autmatically as result
(define A (make-accumulator 5))
(output (A 10)) ; 15
(output (A 10)) ; 25
