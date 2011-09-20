; Exercise 2.7.
;
; Alyssa's program is incomplete because she has not specified the implementation 
; of the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

; Define selectors upper-bound and lower-bound to complete the implementation.
; -----------------------------------------------

; loading already given definitions

(load "2.1.scm")

; defining the interval selectors on pairs.
(define (lower-bound x)
  (car x))

(define (upper-bound x)
  (cdr x))

; testing with a few vallues

(define r1 (make-interval 0.15 0.2))
(define r2 (make-interval 0.9 1.1))

; (display (lower-bound (add-interval r1 r2)))
; (newline)
