; Exercise 3.53.  Without running the program, describe the elements of
; the stream defined by

; (define s (cons-stream 1 (add-streams s s)))
; ------------------------------------------------------------

(load "../helpers.scm")
(load "3.5.scm")

(define s (cons-stream 1 (add-streams s s)))

; This produces same result as doubling the stream.
; It makes every next element twice of the previous one

(output (stream-ref s 6)) ; 64
