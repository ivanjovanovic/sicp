; Exercise 3.54.  Define a procedure mul-streams, analogous to
; add-streams, that produces the elementwise product of its two input
; streams. Use this together with the stream of integers to complete the
; following definition of the stream whose nth element (counting from 0)
; is n + 1 factorial:

; (define factorials (cons-stream 1 (mul-streams <??> <??>)))
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorial (cons-stream 1 (mul-streams factorial (stream-cdr integers))))

(output (stream-ref factorial 3))
