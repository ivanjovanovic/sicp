; Exercise 5.20.  Draw the box-and-pointer representation and the
; memory-vector representation (as in figure 5.14) of the list structure
; produced by

(define x (cons 1 2))
(define y (list x x))

; with the free pointer initially p1. What is the final value of free ?
; What pointers represent the values of x and y ?
; ------------------------------------------------------------

; There is a PNG drawing of the memory layout for the creation of these
; two objects x and y.
;
; I start counting from 0 and at the end free register should point to
; the fourth place in the array which is indexed with the number 3.
