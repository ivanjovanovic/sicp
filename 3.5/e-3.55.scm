; Exercise 3.55.
;
; Define a procedure partial-sums that takes as argument
; a stream S and returns the stream whose elements are S0, S0 + S1, S0 +
; S1 + S2, .... For example, (partial-sums integers) should be the
; stream 1, 3, 6, 10, 15, ....
; ------------------------------------------------------------


(load "../helpers.scm")
(load "3.5.scm")

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (partial-sums stream) (stream-cdr stream))))

(display-stream (partial-sums integers)) ; 1 3 6 10 15 21 28 36 45 55 ...
