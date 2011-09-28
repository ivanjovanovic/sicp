; Exercise 2.18.
; Define a procedure reverse that takes a list as argument and returns a list of the same elements in reverse order:
;
; Example: (reverse (list 1 4 9 16 25)) -> (25 16 9 4 1)
; ----------------------------------------------------

(load "../common.scm")

; For me, this would be the natural way of doing it
; but the way cons work doesn't actually allow this to be represented in
; some nice way.
(define (reverse items)
  (if (null? (cdr items))
    (car items)
    (cons (reverse (cdr items)) (car items))))

(display (reverse (list 1 2 3 4))) ; -> (((4 . 3) . 2) . 1)

; If we want proper representation of the list we have to 
; use helper method append and work with the lists
(define (reverse items)
  (if (null? (cdr items))
    (list (car items))
    (append (reverse (cdr items)) (list (car items)))))

(display (reverse (list 1 4 9 16 25))) ; -> (25 16 9 4 1)

; at the end it can be done a bit simpler but with one more recursion
; call
(define (reverse items)
  (if (null? items)
    nil
    (append (reverse (cdr items)) (list (car items)))))

(display (reverse (list 1 4 9 16 25))) ; -> (25 16 9 4 1)
