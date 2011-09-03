; Exercise 1.40.
;
; Define a procedure cubic that can be used together with the newtons-method
; procedure in expressions of the form
;
; (newtons-method (cubic a b c) 1)
;
; to approximate zeros of the cubic x^3 + ax^2 + bx + c.
;
; --------------------------------------------------------


(load "1.3.scm")
(load "../common.scm")

; we define a procedure with three params a, b and c which will
; produce new procedure that has one argument x and will satisfy given
; expression.
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

; (display (newtons-method (cubic 1 2 3) 1)) ; -1.27 approx
; (newline)



