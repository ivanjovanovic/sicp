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

(define ramanujan-weight
  (lambda (pair)
    (let ((i (car pair))
          (j (cadr pair)))
      (+ (* i i i) (* j j j)))))

(define ramanujan-weighted-pairs
  (weighted-pairs
    integers
    integers
    ramanujan-weight))

(display-stream-head ramanujan-weighted-pairs 10)

(define (ramanujan-stream-filter stream)
  (let ((weight1 (ramanujan-weight (stream-car stream)))
        (weight2 (ramanujan-weight (stream-car (stream-cdr stream)))))
    (cond ((stream-null? stream) the-empty-stream)
          ((= weight1 weight2)
           (cons-stream
             (stream-car stream)
             (cons-stream
               (stream-car (stream-cdr stream))
               (ramanujan-stream-filter (stream-cdr (stream-cdr stream))))))
          (else (ramanujan-stream-filter (stream-cdr stream))))))


(define ramanujan-pairs
  (ramanujan-stream-filter ramanujan-weighted-pairs))

(display-stream-head ramanujan-pairs 10)
