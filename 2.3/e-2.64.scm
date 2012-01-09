; Exercise 2.64.  The following procedure list->tree converts an ordered list
; to a balanced binary tree. The helper procedure partial-tree takes as
; arguments an integer n and list of at least n elements and constructs a
; balanced tree containing the first n elements of the list. The result
; returned by partial-tree is a pair (formed with cons) whose car is the
; constructed tree and whose cdr is the list of elements not included in the
; tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; a. Write a short paragraph explaining as clearly as you can how partial-tree
; works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

; b. What is the order of growth in the number of steps required by list->tree
; to convert a list of n elements?
; ------------------------------------------------------------

; a.
;
; Partial tree calculates middle of the list and procedes recurring on the left side of the
; tree by continuously breaking tree on half and proceeding to the left. With the right part if defines a node and
; procedes on the right side by recurring and repeating all the way down until there is no more list elements
; in the set of first n to break on half. If we look from the bottom then it constructs tree from left and 
; right side and returns the remaining elements. At the end on top it ocnstructs the rot of the tree and gets all the 
; remaining after n elements.

; This works only with the ordered trees, since we presume it.


; b.
;
; If we take to build 2 times bigger tree, we'll have to build them both and do one (cons this-entry left-tree right-tree) to
; construct the final result
;
; T(n) = 2*T(n/2) + O(1)
;
; according to Master Theorem this is O(n) order of growth
