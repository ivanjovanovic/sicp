(load "../helpers.scm")
(load "3.5.scm")

; Exercise 3.68.
;
; Louis Reasoner thinks that building a stream of pairs
; from three parts is unnecessarily complicated. Instead of separating
; the pair (S0,T0) from the rest of the pairs in the first row, he
; proposes to work with the whole first row, as follows:

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))

; Does this work? Consider what happens if we evaluate (pairs integers
; integers) using Louis's definition of pairs.
; ------------------------------------------------------------

; Example (display-stream-head (pairs integers integers) 20)
;
; shows that this ends up in the infinite loop.
; Problem with this approach is that we are not putting (interleave)
; in the delayed execution but executing immediatelly which is bringing
; us to infinite loop within (pairs) procedure.
