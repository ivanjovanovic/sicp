; Exercise 2.80.
;
; Define a generic predicate =zero? that tests if its argument
; is zero, and install it in the generic arithmetic package. This operation
; should work for ordinary numbers, rational numbers, and complex numbers.
; ------------------------------------------------------------

(load "2.5.scm")
(load "e-2.78.scm") ; to use normal scheme numbers instead of typed
; Similar to the previous taks, now we just have operation on one operand.

(define (=zero? x) (apply-generic '=zero? x))

; and to update packages
(define (update-scheme-number-package)
  (put '=zero? '(scheme-number) (lambda (x) (= 0 x))))

(update-scheme-number-package)

; this needs access to internal procedures of the package which is
; different story
(define (update-rational-package)
  (put '=zero? '(rational)
       (lambda (x) (= 0 (car x)))))

(update-rational-package)

(define (update-complex-package)
  (put '=zero? '(complex)
       (lambda (c1) (and (= 0 (real-part c1)) (= 0 (imag-part c1))))))

(update-complex-package)


; and some tests

(output (=zero? 0))
(output (=zero? (make-rational 0 5)))
(output (=zero? (make-complex-from-real-imag 0 0)))
