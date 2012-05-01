; Exercise 3.72.
;
; In a similar way to exercise 3.71 generate a
; stream of all numbers that can be written as the sum of two
; squares in three different ways (showing how they can be so
; written).
; ------------------------------------------------------------

(load "e-3.70.scm")

(define square-weight
  (lambda (pair)
    (let ((i (car pair))
          (j (cadr pair)))
      (+ (* i i) (* j j)))))

(define square-weighted-pairs
  (weighted-pairs
    integers
    integers
    square-weight))

;this can be done by applying twice some more general procedure of ramanujan-stream-filter
;which will produce stream of values we need. Here is implementation for current case
(define (square-weight-filter stream)
  (let* ((tail-of-stream (stream-cdr stream))
         (weight1 (square-weight (stream-car stream)))
         (weight2 (square-weight (stream-car tail-of-stream)))
         (weight3 (square-weight (stream-car (stream-cdr tail-of-stream)))))
    (if (and (= weight1 weight2) (= weight2 weight3))
      (cons-stream
        weight1
        (cons-stream
          (stream-car stream)
          (cons-stream
            (stream-car tail-of-stream)
            (cons-stream
              (stream-car (stream-cdr tail-of-stream))
              (square-weight-filter (stream-cdr (stream-cdr tail-of-stream)))))))
      (square-weight-filter tail-of-stream))))

(define square-weight-numbers (square-weight-filter square-weighted-pairs))
(display-stream-head square-weight-numbers 100)
