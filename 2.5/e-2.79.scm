; Exercise 2.79.  
;
; Define a generic equality predicate equ? that tests the
; equality of two numbers, and install it in the generic arithmetic package.
; This operation should work for ordinary numbers, rational numbers, and
; complex numbers.
; ------------------------------------------------------------

(load "2.5.scm")
(load "e-2.78.scm") ; to use normal scheme numbers instead of typed


; This is one case where we have to add a generic procedure that affects all implementations.
; First we have to put the way we will call it, and then the way particular type of number
; will interpret it.

(define (equ? x y) (apply-generic 'equ? x y))

; and we have to tell which procedure should be used for the particular number type.
; We will make update procedures for every package
(define (update-scheme-number-package)
  (put 'equ? '(scheme-number scheme-number) equal?))

(update-scheme-number-package)

; this needs access to internal procedures of the package which is
; different story
(define (update-rational-package)
  (put 'equ? '(rational rational)
       (lambda (x y) (and (equal? (car x) (car y))
                          (equal? (cdr x) (cdr y))))))

(update-rational-package)

(define (update-complex-package)
  (put 'equ? '(complex complex)
       (lambda (c1 c2) (and (equal? (real-part c1) (real-part c2))
                            (equal? (imag-part c1) (imag-part c2))))))

(update-complex-package)

; now some tests
(output
  (equ? (make-complex-from-real-imag 3 4)
        (make-complex-from-real-imag 3 4)))

(output
  (equ? (make-rational 3 4)
        (make-rational 3 4)))

(output
  (equ? 5 5))
