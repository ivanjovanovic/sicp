(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; (display (new-if (> 2 3) 5 6))

; Square root expressed in terms of new-if method

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (display (sqrt 36))

; Although we would expect same result of our function as we would
; with primitive if operator, we get the information that Stack Level
; got too deep during execution.
;
; Difference is that new-if method evaluates in the normal order
; comparing to applicative order of evaluation which is applied to the
; primitive procedures.
;
; Normal order is such that compound procedures are evaluated but
; arguments are not until whole statement is comprised only of primitive
; operations. In this case recursion doesn't end and sqrt-iter is
; evaluated repeatedly until we get to Stack Level too deep message.
; If parameters where to be evaluated in every step, in one moment
; good-enough? would return true and loop would be stopped and `rolled
; back` to root of recursion.
