; Exercise 2.63.
;
; Each of the following two procedures converts a binary
; tree to a list.

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; a. Do the two procedures produce the same result for every tree? If
; not, how do the results differ? What lists do the two procedures
; produce for the trees in figure 2.16?

; b. Do the two procedures have the same order of growth in the number
; of steps required to convert a balanced tree with n elements to a
; list? If not, which one grows more slowly?
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.3-binary-trees.scm")

; a. Lets check what they produce for given examples in the figure 2.16 so
; we can analyze a bit

(define (terminus x)
  (list x '() '()))

(define tree1 
  (list 7 
        (list 3 
              (terminus 1)
              (terminus 5))
        (list 9 
              '()
              (list 11))))

(define tree2
  (list 3
        (terminus 1)
        (list 7
              (terminus 5)
              (list 9
                    '()
                    (terminus 11)))))

(define tree3
  (list 5
        (list 3
              (terminus 1)
              '())
        (list 9
              (terminus 7)
              (terminus 11))))

; (output (tree->list-1 tree1))
; (output (tree->list-2 tree1))

; (output (tree->list-1 tree2))
; (output (tree->list-2 tree2))

; (output (tree->list-1 tree3))
; (output (tree->list-2 tree3))

; They obviously produce same results, they are implemented to traverse th tree
; with respect to its implementation and order of node branches.

; b. Orders of growth are different.
;
; To analyze first one we can think of what it takes to finish job for two trees of the same size.
; It will take T(n) = 2*T(n/2). 
; But then job is not done because we need to append result of first to result of second to make it final which is n more steps
; since append has O(n) order of growth. Finally we have
;
; T(n) = 2*T(n/2) + n steps. When this is solved for n -> infinity we get order of growth
; O(n*logn)
; Check http://en.wikipedia.org/wiki/Merge_sort#Analysis for more details since this is similar to merge sort.
;
;
; In second case we have execution of the copy-to-list n times for every node with cons
; which grows in constant time
;
; T(n) = 2*T(n/2) + O(1) 
; Calculated this is O(n) - linear growth which is faster than O(n*logn)
