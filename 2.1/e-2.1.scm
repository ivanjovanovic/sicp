; Exercise 2.1. 
;
; Define a better version of make-rat that handles both positive and negative arguments.
; make-rat should normalize the sign so that if the rational number is positive, both the numerator and
; denominator are positive, and if the rational number is negative, only the numerator is negative. 

(load "2.1.scm")

; very simple solution to this problem which normalizes the minus sign
; as required
; 
;
(define (make-rat n d)
  (if (< d 0)
    (cons (- n) (- d))
    (cons n d)))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 2))
(print-rat (make-rat 1 -2))
(print-rat (make-rat -1 -2))

