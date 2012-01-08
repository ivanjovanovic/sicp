; Symbols are defined by quoting 

(load "../common.scm")
(load "../helpers.scm")

; Binary tree can be represented by the following abstraction given in
; the form of constructor and selectors.

(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

; given the definition of the tree we can make the procedure that
; searches to see if one element is element of the set.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

; adding a new element of the set

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


; couple of helper methods to generate random numerical trees for testing.
; Running pretty slow though for larger trees
(define (random-list n)
  (do ((random-list '() (cons random-element random-list))
       (random-element (random n) (random n))
       (i 0 (+ i 1)))
    ((= i n) random-list)))

; (output (random-list 1000))

(define (random-tree n)
  (do ((tree-elements (random-list n) (cdr tree-elements))
       (random-tree 
         '() 
         ((lambda (x) (if (element-of-set? x random-tree)
                       random-tree
                       (adjoin-set x random-tree))) (car tree-elements))))
    ((null? tree-elements) random-tree)))

; (output (random-tree 100))

