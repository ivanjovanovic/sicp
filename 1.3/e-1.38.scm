; Exercise 1.38.
;
; In 1737, the Swiss mathematician Leonhard Euler published a memoir
; De Fractionibus Continuis, which included a continued fraction expansion
; for e - 2, where e is the base of the natural logarithms. In this fraction,
; the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....
; Write a program that uses your cont-frac procedure from exercise 1.37 to
; approximate e, based on Euler's expansion.

(load "e-1.37.scm")

(define n-proc
  (lambda (i) 1))

(define d-proc
  (lambda (i)
    (cond ((= i 1) 1)
          ((= i 2) 2)
          (else
            (if (> (remainder (- i 2) 3) 0)
              1
              (* 2 (+ (/ (- i 2) 3.0) 1)))))))


(define e-2
  (cont-frac-iter n-proc d-proc 100))

; calculates approximation of e as required
; (display (+ e-2 2))
; (newline)

