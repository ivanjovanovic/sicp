; Exercise 2.24.
;
; Suppose we evaluate the expression (list 1 (list 2 (list 3 4))).
; Give the result printed by the interpreter,
; the corresponding box-and-pointer structure,
; and the interpretation of this as a tree (as in figure 2.6).
; ------------------------------------------------------------------

(load "../helpers.scm")

(output (list 1 (list 2 (list 3 4)))) ; > (1 (2 (3 4)))

; box-and-pointer representation
;
; |·| | -> |·| | -> | · | · |
;  1        2         3   4
;
; cdr of first is pointer to next list in which cdr is pointer to last
; pair (3 4)
;
; Tree representation
;
;         (1 (2 (3 4)))
;              /\
;             /  \ (2 (3 4))
;            1    |\
;                 | \
;                 2  \ (3 4)
;                     |\
;                     | \
;                     3  4
