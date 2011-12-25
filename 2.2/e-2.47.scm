; Exercise 2.47.  Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))


; For each constructor supply the appropriate selectors to produce an
; implementation for frames.
; -------------------------------------------------- 

(load "../helpers.scm")
(load "e-2.46.scm")

; Selectors have to pull appropriate element out of the representation.
; Point here is that interface towards outside world wil be the same in
; both cases, only internals are going to be different.

; in first case
(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define test-frame 
  (make-frame
    (make-vect 0 0)
    (make-vect 0.5 0.5)
    (make-vect 1 1)))

(output (origin-frame test-frame))
(output (edge1-frame test-frame))
(output (edge2-frame test-frame))

; In second case
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; we have to redefine only last selector to take second cdr directly
; instead of pulling it out from the list with additional car
(define (edge2-frame frame)
  (cddr frame))


