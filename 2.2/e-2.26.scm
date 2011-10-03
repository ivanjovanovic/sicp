; Exercise 2.26.
; Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

; What result is printed by the interpreter in response to evaluating each of the following expressions:

; (append x y)

; (cons x y)

; (list x y)
; --------------------------------------

; without calculation I suppose it should be
;
; 1) (1 2 3 4 5 6)
; 2) ((1 2 3) 4 5 6)
; 3) ((1 2 3) (4 5 6))

; and lets check if that is true

(load "../helpers.scm")

(output (append x y))
(output (cons x y))
(output (list x y))

