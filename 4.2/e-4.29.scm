; Exercise 4.29.  Exhibit a program that you would expect to run much
; more slowly without memoization than with memoization. Also, consider
; the following interaction, where the id procedure is defined as in
; exercise 4.27 and count starts at 0:

(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
<response>
;;; L-Eval input:
count
;;; L-Eval value:
<response>

; Give the responses both when the evaluator memoizes and when it does
; not.

; ------------------------------------------------------------

; Every program that would have passed argument whose value is result of
; some intensive computatio and the procudure that uses the argument often
; would have significant impact on performance without memoization.

; If we don't have memoization in this case count would be immediately 2
; because square would evaluate (id 10) two times for application of the
; primitive procedure *
