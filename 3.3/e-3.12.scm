; Exercise 3.12.
;
; The following procedure for appending lists was
; introduced in section 2.2.1:

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

; Append forms a new list by successively consing the elements of x onto
; y. The procedure append! is similar to append, but it is a mutator
; rather than a constructor. It appends the lists by splicing them
; together, modifying the final pair of x so that its cdr is now y. (It
; is an error to call append! with an empty x.)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

; Here last-pair is a procedure that returns the last pair in its
; argument:

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; Consider the interaction

; (define x (list 'a 'b))
; (define y (list 'c 'd))
; (define z (append x y))
; z
; (a b c d)
; (cdr x)
; <response>
; (define w (append! x y))
; w
; (a b c d)
; (cdr x)
; <response>

; What are the missing <response>s? Draw box-and-pointer diagrams to
; explain your answer.
; ------------------------------------------------------------

(load "../helpers.scm")

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

; Here, append procedure produces fully new list that is constructed
; from combination of old ones in a certain way. Therefore, x and y stay
; not changed, and z is fully different entity here
; (output (cdr x)) ; (b)

; Lets redefine z with mutabe append.
;
; What we did here is to rewire x so its last cdr points
; to y and therefore x is changed. Z is only pointing to the head
; of list x, so it is kind of alias now for x since x is what we 
; returned from (append!) procedure. Comparing to previous method, we
; did not create new list, but just rewired existing ones.
(define z (append! x y))
; (output (cdr x)) ; (b c d)

; box-and-pointers left on the paper. It is not that hard.
