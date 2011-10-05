; Exercise 2.32.
;
; We can represent a set as a list of distinct elements, and we can
; represent the set of all subsets of the set as a list of lists.
; For example, if the set is (1 2 3), then the set of all subsets is
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following
; definition of a procedure that generates the set of subsets of a set
; and give a clear explanation of why it works:

(load "../helpers.scm")
(load "../common.scm")

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; gives expected result (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
(output (subsets (list 1 2 3)))

; The recursive algorithm that is implemented here can be explained in
; following way suitable for recursive implementation:
;
; To list all the subsets of the definite set you need
; 1. All the subsets of a given set without a certain element
; 2. You have to add to this all the subsets from point 1
;    but with that element added to the sets.
;
; More on this is written: http://en.wikipedia.org/wiki/Power_set#Algorithms
