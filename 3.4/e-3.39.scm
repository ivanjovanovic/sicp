(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

; This creates two concurrent processes -- P1, which sets x to x times
; x, and P2, which increments x. After execution is complete, x will be
; left with one of five possible values, depending on the interleaving
; of the events of P1 and P2:
; 101: P1 sets x to 100 and then P2 increments x to 101.
; 121: P2 increments x to 11 and then P1 sets x to x times x.
; 110: P2 changes x from 10 to 11 between the two times that P1 accesses the value of x
;       during the evaluation of (* x x).
; 11: P2 accesses x, then P1 sets x to100, then P2 sets x.
; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.


; Exercise 3.39.  Which of the five possibilities in the parallel
; execution shown above remain if we instead serialize execution as
; follows:

(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))


; ------------------------------------------------------------

; All solutions in which second procedure was interrupted in the middle
; of execution are now prevented. So it will be possible to have only
; cases
; 101: P1 sets x to 100 and then P2 increments x to 101.
; 121: P2 increments x to 11 and then P1 sets x to x times x.
; 100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.
