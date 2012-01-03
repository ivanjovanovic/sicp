; Exercise 2.62.
;
; Give a O(n) implementation of union-set for sets
; represented as ordered lists.
; ------------------------------------------------------------

; in order to achieve this we have to use the fact our lists are ordered
; and to decide to reduce the problem as early as possible to achieve
; the best efficiency.

; Union is defined as set of all elements that can be found in first or
; in the second set.

(load "../helpers.scm")

; set has to remain ordered so in implementation we take the smaller one
; to cons it up, and then reduce the problem to the smaller subsets.
; Ordering maintains the uniqueness so I don't have to check every time
; if there is a element in the set.
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))

; (output (union-set (list 1 2 3) (list 1 2 3)))
; (output (union-set (list 1 2 3) (list 2 3 4)))
; (output (union-set (list 2 3 4) (list 1 2 3)))
; (output (union-set (list 2 3 4 5 6 7) (list 1 2 3 8)))
