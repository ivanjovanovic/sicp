; Exercise 2.60.
;
; We specified that a set would be represented as a list
; with no duplicates. Now suppose we allow duplicates. For instance, the
; set {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design
; procedures element-of-set?, adjoin-set, union-set, and
; intersection-set that operate on this representation. How does the
; efficiency of each compare with the corresponding procedure for the
; non-duplicate representation? Are there applications for which you
; would use this representation in preference to the non-duplicate one?
; ------------------------------------------------------------


(load "../common.scm")
(load "../helpers.scm")
(load "2.3.scm")

; element of set doesn't change since we have to check through set
; anyway
(define (element-of-set? x set)
  (cond ((equal? set '()) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; (output (element-of-set? 5 (list 2 3 4 1 1 1)))

; Adjoin set is different since it doesn't need to check for presence

(define (adjoin-set x set)
  (cons x set))

; (output (adjoin-set 1 (list 1 2 3)))

; Intersection is the same procedure.

(define (intersection-set set1 set2)
  (cond ((or (equal? set1 '()) (equal? set2 '())) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(output (intersection-set (list 1 2 3 1 1 7 7) (list 2 3 4 1 1 7)))

; union is now just appending elements together

(define (union-set set1 set2)
  (append set1 set2))

; (output (union-set (list 1 1 2 3 4) (list 1 1 7 7 7)))

; Given that the only difference is at adjoining elements which is done
; in constant time instead of linear as with non-repeateable sets, we
; could use this when we have to add a lot of elements to set and
; repeating is not important
;
; This solution doesn't go into details of implementaion of multisets
; and their arithmetics and rules. 
; More on multisets here: http://en.wikipedia.org/wiki/Multiset

