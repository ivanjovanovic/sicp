; Exercise 3.64.  Write a procedure stream-limit that takes as arguments
; a stream and a number (the tolerance). It should examine the stream
; until it finds two successive elements that differ in absolute value
; by less than the tolerance, and return the second of the two elements.
; Using this, we could compute square roots up to a given tolerance by

; (define (sqrt x tolerance)
;   (stream-limit (sqrt-stream x) tolerance))

; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")

(define (stream-limit stream tolerance)
  (let ((first  (stream-ref stream 0))
        (second (stream-ref stream 1)))
    (if (< (abs (- second first)) tolerance)
      second
      (stream-limit (stream-cdr stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; these will all provide result with different precision
(output (sqrt 10 0.1)) ; 3.16245562280389
(output (sqrt 10 0.01)) ; 3.16227766517567
(output (sqrt 10 0.00001)) ; 3.16227766016838
