; Exercise 5.1.  Design a register machine to compute factorials using
; the iterative algorithm specified by the following procedure. Draw
; data-path and controller diagrams for this machine.

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

; ------------------------------------------------------------

; Here is in fact the most important do design the internal iterative
; process. My solution is given in the scan e-5.1.png in the same
; folder.
