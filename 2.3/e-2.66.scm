; Exercise 2.66.
;
; Given the generic lookup procedure for the element in the set.
;
; (define (lookup given-key set-of-records)
;   (cond ((null? set-of-records) false)
;         ((equal? given-key (key (car set-of-records)))
;          (car set-of-records))
;         (else (lookup given-key (cdr set-of-records)))))
;
; Implement the lookup procedure for the case where the
; set of records is structured as a binary tree, ordered by the
; numerical values of the keys.
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.3-binary-trees.scm")

; a. Lets check what they produce for given examples in the figure 2.16 so
; we can analyze a bit

(define (terminus x)
  (list x '() '()))

; consider we have at each node pair of (key . values) where
; (car record) returns key, we can make it like this
(define (lookup given-key set-of-records)
  (let ((key (car (entry set-of-records))))
    (cond ((null? set-of-records) false)
          ((equal? given-key key) (entry set-of-records))
          ((> given-key key) (lookup given-key (right-branch set-of-records)))
          ((< given-key key) (lookup given-key (left-branch set-of-records))))))


; testing the solution with an arbitrary tree
(define tree
  (list (cons 7 (list 8 9))
        (list (cons 3 (list 4 5))
              (terminus (cons 1 (list 2 3)))
              (terminus (cons 5 (list 6 7))))
        (list (cons 9 (list 10 11))
              '()
              (terminus (cons 11 (list 12 13))))))


; (output (lookup 7 tree))
; (output (lookup 3 tree))
; (output (lookup 1 tree))
; (output (lookup 5 tree))
; (output (lookup 9 tree))
; (output (lookup 11 tree))

