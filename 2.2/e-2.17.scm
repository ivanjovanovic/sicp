; Exercise 2.17.
; Define a procedure last-pair that returns the list that contains only the
; last element of a given (nonempty) list:
;
; Example: (last-pair (list 10 12 41 32)) -> 32
; -------------------------------------------------

(load "2.2.scm")

; 1) If I would use already defined length procedure.

(define (last-pair items)
  (if (= (length items) 1)
    (car items)
    (last-pair (cdr items))))

(display (last-pair (list 10 12 41 32))) ; 32

; 2) Without any additional helper methods

(define (last-pair items)
  (if (null? (cdr items))
    (car items)
    (last-pair (cdr items))))

(display (last-pair (list 10 12 41 32))) ; 32
