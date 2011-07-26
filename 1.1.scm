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

; exercise 1.2 equation  in prefix form
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3)))) (* 3 (- 6 2) (- 2 7))))

; excercise 1.3
; Define procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers
(define (sum-larger-square a b c)
  (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
        ((and (< b a) (< b c)) (+ (* a a) (* c c)))
        (else (+ (* a a) (* b b)))))

; Exercise 1.4 Operators can be compound expressions. Based on the value of parameter b operator is determined.
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Exercise 1.5 Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using 
; applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; then he tests the interpreter with
(test 0 (p))

; Point is that in normal-order evaluation (p) will try to expand itself even if it doesn't need to
; be evaluated by the interpreter at all for these conditions.
; problem is that (p) expands to itself and then ends up in recursion.
;
; On the other hand, applicative-order evaluation will evaluate argument y which is (p) only
; if it is needed for evaluatin of test procedure with given params. Therefore with this 
; evaluation strategy we will have no problems getting the result from given test.


; Newton square roots finder by iterative guessing

(define (square-iter guess x)
  (if (good-enough? guess x)
      guess
      (square-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (square-iter 1.0 x))

(display (sqrt (+ 32 32)))
