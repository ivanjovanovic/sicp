; Exercise 3.70.
;
; It would be nice to be able to generate streams in
; which the pairs appear in some useful order, rather than in the order
; that results from an ad hoc interleaving process. We can use a
; technique similar to the merge procedure of exercise 3.56, if we
; define a way to say that one pair of integers is ``less than''
; another. One way to do this is to define a ``weighting function''
; W(i,j) and stipulate that (i1,j1) is less than (i2,j2) if W(i1,j1) <
; W(i2,j2). Write a procedure merge-weighted that is like merge, except
; that merge-weighted takes an additional argument weight, which is a
; procedure that computes the weight of a pair, and is used to determine
; the order in which elements should appear in the resulting merged
; stream. Using this, generalize pairs to a procedure weighted-pairs
; that takes two streams, together with a procedure that computes a
; weighting function, and generates the stream of pairs, ordered
; according to weight. Use your procedure to generate

; a. the stream of all pairs of positive integers (i,j) with i < j
; ordered according to the sum i + j

; b. the stream of all pairs of positive integers (i,j) with i < j,
; where neither i nor j is divisible by 2, 3, or 5, and the pairs are
; ordered according to the sum 2 i + 3 j + 5 i j.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")

(define (merge-weighted s t weight)
  (cond ((stream-null? s) t)
        ((stream-null? t) s)
        (else
         (let ((s-car (stream-car s))
               (t-car (stream-car t)))
           (cond ((< (weight s-car) (weight t-car))
                  (cons-stream s-car (merge-weighted (stream-cdr s) t weight)))
                 ((> (weight s-car) (weight t-car))
                  (cons-stream t-car (merge-weighted s (stream-cdr t) weight)))
                 (else
                  (cons-stream s-car (cons-stream t-car ; important is not to forget both elements
                               (merge-weighted (stream-cdr s)
                                               (stream-cdr t)
                                               weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define basic-sum
  (lambda (pair) (+ (car pair) (cadr pair))))

(define complex-sum
  (lambda (pair)
      (let ((i (car pair))
            (j (cadr pair)))
        (+ (* 2 i) (* 3 j) (* 5 i j)))))

(define basic-sum-weighted-pairs
  (weighted-pairs
    integers
    integers
    basic-sum))

(define complex-sum-weighted-pairs
  (weighted-pairs
    integers
    integers
    complex-sum))

; (output "Simple sum ordering (i, j) -> i + j")
; (display-stream-head basic-sum-weighted-pairs 20)
; (output "Complex sum ordering (i, j) -> 2*i + 3*j + 5*i*j")
; (display-stream-head complex-sum-weighted-pairs 20)

