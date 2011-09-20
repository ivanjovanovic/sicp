; Exercise 2.9.
;
; The width of an interval is half of the difference between its upper and lower bounds. 
; The width is a measure of the uncertainty of the number specified by the interval. 
; For some arithmetic operations the width of the result of combining two intervals 
; is a function only of the widths of the argument intervals, whereas for others the width 
; of the combination is not a function of the widths of the argument intervals. Show that 
; the width of the sum (or difference) of two intervals is a function only of the widths 
; of the intervals being added (or subtracted). Give examples to show that this is not true 
; for multiplication or division.
; ----------------------------------------------------

; we can mathematicaly prove that sum-interval and sub-interaval produce
; forms where widths depend only on the widths of intervals.

; r1 = c1 - w1, c1 + w1
; r2 = c2 - w2, c2 + w2
;
; r1 + r2 = c1 - w1 + c2 - w2, c1 + w1 + c2 + w2
; => c1 + c2 - (w1 + w2), c1 + c2 + (w1 + w2)
;
; r3 = c3 - w3, c3 + w3 ; where c3 = c1 + c2 and w3 = w1 + w2
;
; Similar we can prove for the substitution.
;
; We can check this on example

(load "2.1.scm")
(load "e-2.7.scm")
(load "e-2.8.scm")

(define r1 (make-interval 0.9 1.1)); c = 1, w = 0.1
(define r2 (make-interval 2.5 3.5)); c = 3, w = 0.5

; what we expect is to get result for summation c = 4, w = 0.6
(display (lower-bound (add-interval r1 r2)))
(newline)
(display (upper-bound (add-interval r2 r1)))
(newline)

; here we expect c = 2, w = 0.4
(display (lower-bound (sub-interval r2 r1)))
(newline)
(display (upper-bound (sub-interval r2 r1)))
(newline)


; on next couple of examples we can show that for multiplication and
; division it is not that linear as before

; lets say that we expect c = c1*c2 and w = w1*w2
; but it is not like that in fact. It can be mathematicaly expressed how
; w depends on c1, c2, w1 and w2
(display (lower-bound (mul-interval r1 r2)))
(newline)
(display (upper-bound (mul-interval r1 r2)))
(newline)
