; Symbols are in computer represented by bits. In general we need
; log2(n) bits to represent n different symbols. Exaple is ASCII set of
; characters which with 7 bits can represent 2^7 or 128 different chars.
;
; Another example is set A B C D E F G H, for these 8 chars we need tree
; bits. A = 000, B = 001 ...
;
; To compose the binary message out of these chars is straightforward.
;
; ABACDE = 000001000010100101
;
; This code is called fixed length code.
;
; Since we don't have to write A as 000 but we can write only 0, we
; could write that in variable length code taking A = 0
;
; ABACDE = 00010010100101
;
; This code is shorter, but for machine understanding it has one
; pitfall, it is ambiguous. We don't really know when one char starts
; and when it ends.
;
; Morse's alphabet uses separator codes for solving this.
;
; Another way is to design code such that no subsequent symbol starts
; with the bit with which previous one starts. One such coding scheme is
; Huffman encoding.
;
; Explanation of how Huffman trees are build is given here
; http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-16.html#%_fig_2.18
;
; Given a Huffman tree we can find appropriate codes for any alphabet
; with given weights and as well decode the bit sequence encoded with
; these symbols.
; More on Huffman encoding and applications on Wikipedia:
; http://en.wikipedia.org/wiki/Huffman_coding#Applications
;
; Huffman as well gave the algorithm to construct the tree in order to
; generate the most efficient encoding scheme.
;
; Representing Huffman trees.
;
; All that said, it is time to build some abstractions to represent the
; knowledge we have about this kind of trees.
;
; From the programming side here is interesting to see the concepts that
; are defined earlier comming to practice.
;
; 1. Defining abstractions so we don't have to deal with the
; representation of data. Building simple constructors, selectors and
; predicates around the data representation.
;
; 2. Building more complex data structures by the means of combinations
; from the simpler ones. Starting from leafs, and building trees that
; combine proprties of their children by the simple means of
; combinations.
;
; 3. Building a language in which we can express our ideas regarding
; this kind of trees, by properly giving names to things.

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf object)
  (cadr object))
(define (weight-leaf object)
  (caddr object))

; General tree node will be a set of left and right branches, list of
; symbols and combined weight of all the symbols

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; and some selectors on the tree

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; algorithm for decoding the bit sequence with the tree

; Here is interesting how we keep in the outside scope reference to the
; whole tree and in the inner scope we drill down the tree. Once we
; decode one symbol we use this reference to restart decoding for the
; next symbol.
(define (decode bits tree)
  (define (decode-inner bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-inner (cdr bits) tree))
          (decode-inner (cdr bits) next-branch)))))
  (decode-inner bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "Something is wrong in your bits -- CHOOSE BRANCH"))))


; Similar to Exercise 2.61 we can make ordered set of symbols by their
; weights, since we'll have to compare them a lot during merging

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; The following procedure takes a list of symbol-frequency pairs such as
; ((A 4) (B 2) (C 1) (D 1)) and constructs an initial ordered set of
; leaves, ready to be merged according to the Huffman algorithm:

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; look at e-2.69
(define (successive-merge tree)
  (if (null? (cdr tree)) 
    tree
    (successive-merge 
      (adjoin-set 
        (make-code-tree (car tree) (cadr tree))
        (cddr tree)))))
