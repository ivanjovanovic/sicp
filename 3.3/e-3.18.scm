; Exercise 3.18.
;
; Write a procedure that examines a list and
; determines whether it contains a cycle, that is, whether a
; program that tried to find the end of the list by taking
; successive cdrs would go into an infinite loop. Exercise 3.13
; constructed such lists.
; ------------------------------------------------------------

; I guess, the way to go there is to go through the list and if
; we end in a situation to get to the pair that has pointer to
; a pair that is already traversed then we are definitelly going to 
; end up cycling through them.

(load "../helpers.scm")
(load "3.3.scm")

; by reusing history object, it is actually simple to write this
(define (cycled? l)
  (cond ((or (null? l) (not (pair? l))) '#f)
        ((null? (cdr l)) '#f)
        (((history 'visited?) (cdr l)) '#t)
        (else
          (begin ((history 'add) l)
                 (or (cycled? (car l))
                  (cycled? (cdr l)))))))

; we have to reset global history object every time.
; This can be improved by making new history local to the cycled? procedure.
(output (cycled? (list 1 2 3))) ; #f
(history 'reset)

(define l (list 1 2 3))
(append! l l) ; cycle

(history 'reset)
(output (cycled? l)) ; #t

(define m (list 1 2 3))
(define n (list 1 2 3))
(append! m n)

(history 'reset)
(output (cycled? m))

(history 'reset)
(output (cycled? n))
