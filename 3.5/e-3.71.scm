; Exercise 3.71.
;
; Numbers that can be expressed as the sum of two cubes in more
; than one way are sometimes called Ramanujan numbers, in honor of the
; mathematician Srinivasa Ramanujan.70 Ordered streams of pairs provide an
; elegant solution to the problem of computing these numbers. To find a number
; that can be written as the sum of two cubes in two different ways, we need
; only generate the stream of pairs of integers (i,j) weighted according to the
; sum i3 + j3 (see exercise 3.70), then search the stream for two consecutive
; pairs with the same weight. Write a procedure to generate the Ramanujan
; numbers. The first such number is 1,729. What are the next five?

(load "e-3.70.scm")
(load "3.5.scm")

(define (ramanujan-weight pair)
    (let ((i (car pair))
          (j (cadr pair)))
      (+ (* i i i) (* j j j))))

(define ramanujan-weighted-pairs
  (weighted-pairs
    integers
    integers
    ramanujan-weight))

; (display-stream-head (stream-map ramanujan-weight ramanujan-weighted-pairs) 100)

(define (ramanujan-stream-filter stream)
  (let* ((weight1 (ramanujan-weight (stream-car stream)))
        (tail-of-stream (stream-cdr stream))
        (weight2 (ramanujan-weight (stream-car tail-of-stream))))
    (if (= weight1 weight2)
      (cons-stream weight1 (ramanujan-stream-filter (stream-cdr tail-of-stream)))
      (ramanujan-stream-filter tail-of-stream))))

(define ramanujan-numbers (ramanujan-stream-filter ramanujan-weighted-pairs))
(display-stream-head ramanujan-numbers 10) ;1729 4104 13832 20683 32832 39312 40033 46683 64232 65728

