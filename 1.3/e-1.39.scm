; Exercise 1.39.
; A continued fraction representation of the tangent function was published
; in 1770 by the German mathematician J.H. Lambert:
;
;                 x
; tan x =  ---------------------
;                       x^2
;           1 -  ---------------
;                         x^3
;                 3 - ----------
;                       5 - ....
;
; where x is in radians.
;
; Define a procedure (tan-cf x k) that computes an approximation to the tangent
; function based on Lambert's formula. K specifies the number of terms to compute,
; as in exercise 1.37.
;

; defining tan-cf and hiding specific procedures inside
(define (tan-cf x k)

  ; copying most of the function which is defined in 1.37
  ; This may be even generalized with passign the sign for the 
  ; recursive arithmetic.
  (define (cont-frac x n-proc d-proc k)
    (define (iter i result)
      (let ((n (n-proc x i))
            (d (d-proc i)))
        (if (= i 1)
          (/ n (- d result))
          (iter (- i 1) (/ n (- d result))))))
    (iter k 1))

  (define n-proc
    (lambda (x i) (exp x i)))

  (define d-proc
    (lambda (i) (- (* 2 i) 1)))

  (cont-frac x n-proc d-proc k))

; (display (tan 2 10)) ; -2.185039863261519
; (newline)
;
;
; Did I say that I like iterative more than recursive definitions.
; Although recursive is easier to reason about. It is as with most
; things in life, if it is easy there must be some catch in it :)

