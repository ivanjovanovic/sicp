(load "../common.scm")

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (>= x y)
  (or (> x y) (= x y)))


; Newton square roots finder by iterative guessing
(define (square-iter guess x)
  (if (good-enough? guess x)
      guess
      (square-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))


(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (square-iter 1.0 x))

; (display (sqrt (+ 32 32)))
