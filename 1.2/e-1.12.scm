; Exercise 1.12.  The following pattern of numbers is called Pascal's triangle.
;
;                rows
;                 1 | 1
;                 2 | 1   1
;                 3 | 1   2   1
;                 4 | 1   3   3   1
;                 5 | 1   4   6   4   1
;                   --------------------
;               cols  1   2   3   4   5
; The numbers at the edge of the triangle are all 1, and each number inside
; the triangle is the sum of the two numbers above it.
; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.
;
; -----------------------------------------------------------------
;
;
; For given row and col 1-based values, we can define this simple
; recursive function.


(define (pascal row col)
  (if (or (= col 1) (= col row))
      1
      (+ 
        (pascal (- row 1) (- col 1))
        (pascal (- row 1) col))))

(display (pascal 10 5))
(newline)
