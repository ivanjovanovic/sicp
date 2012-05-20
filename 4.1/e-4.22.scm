; Exercise 4.22.
;
; Extend the evaluator in this section to support the special
; form let. (See exercise 4.6.)
; ------------------------------------------------------------

(load "e-4.6.scm")

; so, in the analyzer we just have to add a line

(define (analyze exp)
  .
  .
  ((let? exp) (analyze (let->combination exp)))
  .
  .)
