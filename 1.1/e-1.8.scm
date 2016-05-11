; Exercise 1.8.
; Newton's method for cube roots is based on the fact that
; if y is an approximation to the cube root of x, then a better approximation is given by the value
; (x/y^2 + 2y) / 3
; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (square x) (* x x))

(define (improve guess x)
  (/
    (+
      (/ x (square guess))
      (* guess 2))
    3))

(define (cube-root-iter guess x)
  (if (in-delta? guess (improve guess x))
      guess
      (begin
        (display guess)
        (newline)
        (cube-root-iter (improve guess x) x))))

(define (in-delta? guess1 guess2)
  (< (abs (- guess1 guess2)) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

; (display (cube-root 8100))
; (newline)


; To solve this escercise we just need to replace the way we improve our
; guess in every new iteration. I renamed the methods so they match the
; problem we are trying to solve.
