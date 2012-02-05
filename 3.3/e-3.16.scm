; Exercise 3.16.
;
; Ben Bitdiddle decides to write a procedure to count
; the number of pairs in any list structure. ``It's easy,'' he reasons.
; ``The number of pairs in any structure is the number in the car plus
; the number in the cdr plus one more to count the current pair.'' So
; Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

; Show that this procedure is not correct. In particular, draw
; box-and-pointer diagrams representing list structures made up of
; exactly three pairs for which Ben's procedure would return 3; return
; 4; return 7; never return at all.
; ------------------------------------------------------------

(load "../helpers.scm")

; The straightforward definition will return actually 3
(define l (list 1 2 3)) ; three pairs, linked in a list
(output (count-pairs l))

(define first (cons 1 2))
(define second (cons 1 2))
(define third (cons first second))
(set-car! second first)
(output (count-pairs third)) ; 4
  
(define first (cons 1 2))
(define second (cons first first))
(define third (cons second second))
(output (count-pairs third))
  
; It will never return if there is a cycle in the data structure.
(define x (list 1 2 3))
(set-cdr! x x) ; three pairs cycled
(output (count-pairs x))



