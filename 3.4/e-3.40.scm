; Exercise 3.40.  Give all possible values of x that can result from
; executing

(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

; Which of these possibilities remain if we instead use serialized
; procedures:

(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))

; ------------------------------------------------------------

; There are 7 different cases that can happen and 5 different values to
; be produced
;
; 1) 1.000.000: P1 sets X to 100, P2 sets X to 1.000.000
; 2) 1.000.000: P2 sets X to 1.000, P1 reads both X and set it to 1.000.000
; 3) 100.000: P2 reads one X, P1 sets X to 100, P2 sets it to 100.000
; 4) 10.000: P1 reads X once, P2 sets it to 1000, P1 sets to 10.000
; 5) 10.000: P2 reads two X, P1 sets X to 100, P2 sets X to 10.000
; 6) 1.000 P2 reads 3 X, P1 sets X o 100, P2 sets X to 1.000
; 7) 100: P1 reads both X, P2 sets X to 1.000, P1 sets it to 100

; with serialization we can have only (10^3)^2 or ((10^2)^3) which both
; give 1.000.000
