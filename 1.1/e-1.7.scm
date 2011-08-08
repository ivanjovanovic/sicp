; Exercise 1.7.  The good-enough? test used in computing square roots will not be very
; effective for finding the square roots of very small numbers. Also, in real computers,
; arithmetic operations are almost always performed with limited precision. This makes our
; test inadequate for very large numbers. Explain these statements, with examples showing
; how the test fails for small and large numbers. An alternative strategy for implementing
; good-enough? is to watch how guess changes from one iteration to the next and to stop when
; the change is a very small fraction of the guess. Design a square-root procedure that uses
; this kind of end test. Does this work better for small and large numbers?

(define (square-iter guess x)
  (if (good-enough? guess x)
      guess
      (begin ; used this compount procedure in order to be able to
        (display guess) ; display the intermediate guess values
        (newline)
        (square-iter (improve guess x) x)
      )
  )
)

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (square-iter 1.0 x))

;(display (sqrt 1234567898765432101010101010))
;(display (sqrt 0.0006))
;(newline)

; additional definitions for new way of implementation of the Babylonian
; method. improve method is called twice since we still did not
; introduce tools to remember local value in the procedure.
(define (square-iter-delta guess x)
  (if (in-delta? guess (improve guess x))
      guess
      (begin
        (display guess)
        (newline)
        (square-iter-delta (improve guess x) x)
      )
  )
)

(define (in-delta? guess1 guess2)
  (< (abs (- guess1 guess2)) 0.001))

(define (sqrt-delta x)
  (square-iter-delta 1.0 x))

;(display (sqrt-delta 1234567898765432101010101010))
(display (sqrt-delta 0.0006))
(newline)


; Ratio behind the problems.
;
; 1. Big numbers:
;
; If we use first method to calculate extremely big number 1234567898765432101010101010
; then we will get this evolution of the guess
;
; (sqrt 1234567898765432101010101010)
;
; 1.0
; 617283949382716100000000000.0
; 308641974691358040000000000.0
; 154320987345679020000000000.0
; 77160493672839510000000000.0
; 38580246836419755000000000.0
; ....
; 43273973866905.25
; 75945403408004.69
; 46100697891822.37
; 36440253602695.43
; 35159744074951.336
; 35136426148530.312
; 35136418411179.758
; 35136418411178.906
; 35136418411178.906
; 35136418411178.906
; 35136418411178.906
; 35136418411178.906
; .... endless iteration
;
; Here we see that at one moment guess stops to refine towards the final solution.
; This is due to how floating-point numbers are represented in computer and the precision
; they can provide in calculations.
; More about that can be found here: http://en.wikipedia.org/wiki/Floating_point
;
; TL;DR: Since all real numbers can be presented only with limited number of significants, at one point
; guess refinement starts needing more significants for greater precision, and then gets into endless loop
; since every next one is the same as previous and still not good enough result to end calculation.
;
; Running it with other method will produce same, incorrect, result but will stop the recursion.
;
; 2. Small numbers
;
; If we use first method to calculate very small number 0.0006 then we will get following
; evolution of the guess, which ends at obviously wrong solution.
;
; 1.0
; 0.5003
; 0.2507496402158705
; 0.1265712325922831
; 0.06565582312636475
; 0.037397194007827136
;
; This is because the good-enough? will stop here because
; | 0.0006 - (0.037397194007827136 * 0.037397194007827136) | is less than 0.001
;
; So here we would need greater precision for the criterium.
;
; Running it with adjusted method will produce 0.024587591941419955 as result
