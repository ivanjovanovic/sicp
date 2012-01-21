; Exercise 2.83.
;
; Suppose you are designing a generic arithmetic
; system for dealing with the tower of types shown in figure 2.25:
; integer, rational, real, complex. For each type (except
; complex), design a procedure that raises objects of that type
; one level in the tower. Show how to install a generic raise
; operation that will work for each type (except complex).
; ------------------------------------------------------------

(load "../helpers.scm")
(load "2.5.scm")
(load "e-2.78.scm")

(define (raise-scheme-number n)
  (make-rational n 1))

  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))

(define (raise-rational n)
  (attach-tag 'real (/ (numer (content n)) (denom (content n)))))

(define (raise-real n)
  (make-complex-from-mag-ang (content n) 0))

; test procedures to how raise works
; (output (raise-scheme-number 5))
; (output (raise-rational (make-rational 3 2)))
; (output (raise-real (raise-rational (make-rational 3 2))))


; here we can define generic raise procedure for our
; arithmetic system and install it

(define (update-scheme-number-package)
  (put 'raise '(scheme-number) raise-scheme-number))

(update-scheme-number-package)

(define (raise-rational-to-complex n)
  (make-complex-from-real-imag (/ (numer n) (denom n)) 0))


; since we dont work with real numbers yet
(define (update-rational-package)
  (put 'raise '(rational) raise-rational-to-complex))

(update-rational-package)

(define (raise n) (apply-generic 'raise n))

; (output (raise 5))
