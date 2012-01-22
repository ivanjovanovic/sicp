; Exercise 2.86.
;
; Suppose we want to handle complex numbers whose real parts,
; imaginary parts, magnitudes, and angles can be either ordinary numbers,
; rational numbers, or other numbers we might wish to add to the system.
; Describe and implement the changes to the system needed to accommodate this.
; You will have to define operations such as sine and cosine that are generic
; over ordinary numbers and rational numbers.
;------------------------------------------------------------

; First to see how things were defined
(load "../helpers.scm")
(load "arithmetics.scm")

; as example I will implement only the sine and cosine generic functions. Other primitive functions
; must be implemented in order tha whole system works with generic solutions.

(define (sine x) (apply-generic 'sine x)) 
(define (cosine x) (apply-generic 'cosine x)) 
 
;; add into scheme-number package 
(put 'sine '(scheme-number) (lambda (x) (sin x)))
(put 'cosine '(scheme-number) (lambda (x) (cos x))) 
 
;; add into rational package 
(put 'sine '(rational) (lambda (x) (attach-tag 'rational (sin (/ (car x) (cdr x))))))
(put 'cosine '(rational) (lambda (x) (attach-tag 'rational (cos (/ (car x) (cdr x))))))

(output (sine 3.14159265))
(output (sine (make-rational 4 2)))

; Instead of + and / we have to use generic add and div
; We would have to implement generic square, atan and to install them
; in packages. This is a bit over too much for this section if you do all the exercises.
; The goal is definitely reached by understanding all up to now :)
