; Exercise 2.31.
;
; Abstract your answer to exercise 2.30 to produce a procedure tree-map
; with the property that square-tree could be defined as

; (define (square-tree tree) (tree-map square tree))
; -------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")

; generalizing traversion of the tree with tree-map function
(define (tree-map mapper tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (mapper tree))
        (else (cons (tree-map mapper (car tree))
                    (tree-map mapper (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))

(output (square-tree (list 1 (list 2 3) 4 (list 5 6))))
(output (square-tree (list 5 (list (list 10 20) (list 10 20)))))

; otherwise, tree mapping can be done by applying map to sequence of sub
; trees
(define (tree-map mapper tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map mapper sub-tree)
           (mapper sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

; this give same results
(output (square-tree (list 1 (list 2 3) 4 (list 5 6))))
(output (square-tree (list 5 (list (list 10 20) (list 10 20)))))

