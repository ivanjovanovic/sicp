; Exercise 2.34.
;
; Evaluating a polynomial in x at a given value of x can be
; formulated as an accumulation. We evaluate the polynomial
; 
; An*x^n + An-1*x^n-1 + ... + A0
;
; using a well-known algorithm called Horner's rule, which structures the computation as
;
; (...(An*x + An-1)*x + ... + A1)*x + A0
;
; In other words, we start with An, multiply by x, add An-1, multiply by x, and so on,
; until we reach A0. Fill in the following template to produce a procedure that evaluates
; a polynomial using Horner's rule. Assume that the coefficients of the polynomial are arranged in a sequence, from A0 through an.

; (define (horner-eval x coefficient-sequence)
;  (accumulate (lambda (this-coeff higher-terms) <??>)
;              0
;              coefficient-sequence))
;
;For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would evaluate
;
;(horner-eval 2 (list 1 3 0 5 0 1))
;----------------------------------------------------------------------

(load "../helpers.scm")
(load "2.2.scm")

(define (horner-eval x coefficient-sequence)
 (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
             0
             coefficient-sequence))

(output (horner-eval 2 (list 1 3 0 5 0 1)))
