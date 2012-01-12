; Exercise 2.69.
;
; The following procedure takes as its argument a list of
; symbol-frequency pairs (where no symbol appears in more than one pair) and
; generates a Huffman encoding tree according to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Make-leaf-set is the procedure given above that transforms the list of pairs
; into an ordered set of leaves. Successive-merge is the procedure you must
; write, using make-code-tree to successively merge the smallest-weight
; elements of the set until there is only one element left, which is the
; desired Huffman tree. (This procedure is slightly tricky, but not really
; complicated. If you find yourself designing a complex procedure, then you are
; almost certainly doing something wrong. You can take significant advantage of
; the fact that we are using an ordered set representation.)
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "example-huffman.scm")
(load "e-2.68.scm")

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; this works but doesn't generate the most optimal tree.
; I made it before finding the most optimal and simple way to do it.
(define (successive-merge tree)
  (cond ((and (leaf? (car tree)) (null? (cdr tree))) (car tree))
        ((null? (cddr tree))
         (make-code-tree (car tree) (cadr tree)))
        (else
          (successive-merge
            (cons (make-code-tree (car tree) (cadr tree))
                  (list (successive-merge (cddr tree))))))))

; It is very simple in fact to generate Huffman tree if you take the right approach.
; Like with all the things in life I guess.
; http://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
(define (successive-merge tree)
  (if (null? (cdr tree)) 
    tree
    (successive-merge 
      ; here we add result of the new tree we made to the list of trees
      (adjoin-set 
        (make-code-tree (car tree) (cadr tree))
        (cddr tree)))))

; (output (make-leaf-set (list (list 'D 1) (list 'C 1) (list 'B 4) (list 'A 5))))
; (A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)
(define encoding-tree 
  (generate-huffman-tree 
    (list 
      (list 'A 8) 
      (list 'B 3) 
      (list 'C 1) 
      (list 'D 1) 
      (list 'E 1)
      (list 'F 1)
      (list 'G 1)
      (list 'H 1))))

(output encoding-tree)
(output (encode '(A B C D) encoding-tree) )
(output (decode '(0 0 0 1 1 1 0 1 1 0 1 0 1 1 0 0) encoding-tree))

; What is here impressive is how the closure property of the nodes of the tree is allowing
; us to make really frictionless algorithm for finding the most efficient tree.
