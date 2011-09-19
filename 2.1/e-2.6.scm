; Exercise 2.6.
;
; In case representing pairs as procedures wasn't mind-boggling enough, consider that,
; in a language that can manipulate procedures, we can get by without numbers (at least insofar
; as nonnegative integers are concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; This representation is known as Church numerals, after its inventor, Alonzo Church,
; the logician who invented the lambda calculus.
;
; Define one and two directly (not in terms of zero and add-1).
; (Hint: Use substitution to evaluate (add-1 zero)). Give a direct definition of the
; addition procedure + (not in terms of repeated application of add-1).
; ----------------------------------------------------------------

; 1. Lets evaluate (add-1 zero) so we can see how one as CL looks like
;
; (add-1 zero)
; (add-1 (lambda (f) (lamda (x) x))) ; first evaluate operand
; ((lambda (f) (lambda (x) (f ((zero f) x))))) ; now apply the operator
; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))) ; reduces to
; ((lambda (f) (lambda (x) (f ((lambda (x) x) x))))) ; reduces to
; (lambda (f) (lambda (x) (f x)))

; Finally we can define numeral 1 as compound procedure.
; It is in fact the same as defined in 
; http://en.wikipedia.org/wiki/Church_encoding#Church_numerals
(define one
  (lambda (f) (lambda (x) (f x))))

; without doing substitution again I can define two as
(define two
  (lambda (f) (lambda (x) (f (f x)))))


; addition n + m is defined by f^n(f^m(x)) so I just have to define it
; as such

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))
