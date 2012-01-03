; Exercise 2.59.
;
; Implement the union-set operation for the
; unordered-list representation of sets.
; ------------------------------------------------------------

; union set is defined as set of elements that are elements of two (or
; more) sets all together

(load "../common.scm")
(load "../helpers.scm")
(load "2.3.scm")

; union of sets represented as unordered lists
(define (union-set set1 set2)
  (cond ((equal? set1 '()) set2)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

; (output (union-set (list 1 2 4) (list 4 5 6)))
