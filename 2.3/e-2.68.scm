; Exercise 2.68.
;
; The encode procedure takes as arguments a message and a tree and produces the
; list of bits that gives the encoded message.

; (define (encode message tree)
;   (if (null? message)
;       '()
;       (append (encode-symbol (car message) tree)
;               (encode (cdr message) tree))))

; Encode-symbol is a procedure, which you must write, that returns the list of
; bits that encodes a given symbol according to a given tree. You should design
; encode-symbol so that it signals an error if the symbol is not in the tree at
; all. Test your procedure by encoding the result you obtained in exercise 2.67
; with the sample tree and seeing whether it is the same as the original sample
; message.
;
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "example-huffman.scm")

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; required procedure to make a solution
(define (encode-symbol symbol tree)
  (if (leaf? tree)
    '()
    (cond ((element-of-set? symbol (symbols (right-branch tree)))
           (cons 1 (encode-symbol symbol (right-branch tree))))
          (else (cons 0 (encode-symbol symbol (left-branch tree)))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(A D A B B C A))

; (output (encode sample-message sample-tree)) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
