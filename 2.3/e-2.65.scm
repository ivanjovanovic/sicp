; Exercise 2.65.  
;
; Use the results of exercises 2.63 and  2.64 to give O(n)
; implementations of union-set and intersection-set for sets implemented as
; (balanced) binary trees.
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.3.scm")
(load "2.3-binary-trees.scm")
(load "e-2.63.scm")
(load "e-2.64.scm")

; Since we have O(n) implementations of list->tree, union-set, intersection-set
; and tree->list then we can just combine them linearly and still have linear order of growth.

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) 
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) (intersection-set (cdr set1) set2))
            ((< x2 x1) (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))
;
; intersection of the sets
(define (tree-intersection tree1 tree2)
  (list->tree (intersection-set (tree->list-2 tree1)
                                (tree->list-2 tree2))))

; for the union as well
(define (tree-union tree1 tree2)
  (list->tree (union-set (tree->list-2 tree1)
                         (tree->list-2 tree2))))

(define r-tree-1 (random-tree 20))
(define r-tree-2 (random-tree 20))

(output r-tree-1)
(output r-tree-2)

(output (tree-union r-tree-1 r-tree-2))
(output (tree-intersection r-tree-1 r-tree-2))
