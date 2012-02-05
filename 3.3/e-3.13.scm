(load "3.3.scm")

; Exercise 3.13.  Consider the following make-cycle procedure, which
; uses the last-pair procedure defined in exercise 3.12:

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

; Draw a box-and-pointer diagram that shows the structure z created by

(define z (make-cycle (list 'a 'b 'c)))

; What happens if we try to compute (last-pair z)?
; ------------------------------------------------------------

(load "../helpers.scm")


; cycle will make this kind of structure
;
; z ---> | a |  | -> | b |  | -> | c |  | -|
;      ^                                   |
;      |-----------------------------------|
;
; 
; So, it will make link from last to first element.
;
; This can not return last pair, since it will loop forever, given the
; condition in (last-pair) procedure
;

