(load "../helpers.scm")
(load "3.5.scm")

; Exercise 3.58.  Give an interpretation of the stream computed by the
; following procedure:

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; (Quotient is a primitive that returns the integer quotient of two
; integers.) What are the successive elements produced by (expand 1 7
; 10) ? What is produced by (expand 3 8 10) ?
; ------------------------------------------------------------

; (display-stream (expand 1 7 10)) ; 1 4 2 8 5 7 1 4 2 8 ..

; (display-stream (expand 3 8 10)) ; 3 7 5 0 0 0 0 0 0 0 ...
