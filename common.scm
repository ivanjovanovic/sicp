; Library of common procedures used in exercises which do not particularly participate
; in solution design but are used only as helpers and can be shared among.

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (div a b)
  (floor (/ a b)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (identity x) x)

(define close-enough?
  (lambda (a b delta) (< (abs (- a b)) delta)))

; average of 2 values
(define (average x y)
  (/ (+ x y) 2.0))

; avergae of 3.0
(define (average-of-3 x y z)
  (/ (+ x y z) 3.0))

(define (inc x)
  (+ x 1))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; define nil as empty list.
(define nil '())
