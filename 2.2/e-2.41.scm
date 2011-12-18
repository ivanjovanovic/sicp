; Exercise 2.41.  
;
; Write a procedure to find all ordered triples of distinct positive integers i, j, and k
; less than or equal to a given integer n that sum to a given integer s.
; --------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.2.scm")

; First we have to create generator of the list of triples, which is
; just one more level of nesting than with 2 levels.
;
; This can be probably abstracted to a method that can build
; recursively any level of nesting. If needed I'll do it later'
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (jk-pair) (append jk-pair (list i)))
                  (flatmap (lambda (j)
                             (map (lambda (k) (list k j))
                                  (enumerate-interval 1 (- j 1))))
                           (enumerate-interval 1 (- i 1)))))
             (enumerate-interval 1 n)))

; helper method to easily get the sum of the triplet
(define (triplet-sum triplet)
  (+ (car triplet) (cadr triplet) (cadr (cdr triplet))))

; append sum to triplet
(define (make-triplet-sum triplet)
  (append triplet (list (triplet-sum triplet))))

; generate filter method that is in fact closure with given s as 
; the enclosed value
(define (triplet-sum-equal? s)
  (lambda (triplet) (= (triplet-sum triplet) s)))

; using the flow of the data to filter out what I need and generate
; proper presentationat the end
(define (sum-equal-triplets n s)
  (map make-triplet-sum
       (filter (triplet-sum-equal? s)
               (unique-triples n))))

(output (sum-equal-triplets 5 10))
