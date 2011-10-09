; Exercise 2.33.
;
; Fill in the missing expressions to complete the following definitions of some
; basic list-manipulation operations as accumulations:

; (define (map p sequence)
;   (accumulate (lambda (x y) <??>) nil sequence))
; (define (append seq1 seq2)
;   (accumulate cons <??> <??>))
; (define (length sequence)
;   (accumulate <??> 0 sequence))
; ----------------------------------------

(load "../helpers.scm")
(load "../common.scm")
(load "2.2.scm")
; Solutions
;
; 1. Map

(define (map p sequence)
  (accumulate 
    (lambda (x y) (cons (p x) y))
    nil
    sequence))

(output (map square (list 1 2 3 4 5)))

; 2. Append

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))


(output (append (list 1 2 3) (list 4 5 6)))

; 3. length

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(output (length (list 1 4 19 100 3425 100 300)))
