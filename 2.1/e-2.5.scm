; Exercise 2.5.
;
; Show that we can represent pairs of nonnegative integers
; using only numbers and arithmetic operations if we represent 
; the pair a and b as the integer that is the product 2a 3b. 
; Give the corresponding definitions of the procedures cons, car, and cdr.
; ---------------------------
;
; Considering that we already have exponential function, cons looks like
; this.

(load "../1.2/1.2.scm")

(define (cons x y)
  (*
    (fast-expt 2 x)
    (fast-expt 3 y)))

; to test if this works
(display (cons 1 2)) ; expected 18
(newline)

; we can get car by iteratively dividing by 2 and counting how many
; times number can be divided

(define (car z)
  (define (iter x count divisor)
    (if (divides? divisor x)
      (iter (/ x divisor) (+ count 1) divisor)
      count))
  (iter z 0 2))

; lets test if this counting of number of factors of 2 works on several
; examples. First one is 

(display (car (cons 1 2))) ; expected 1
(newline)

(display (car (cons 2 3))) ; expected 2
(newline)

(display (car (cons 22 2))) ; expected 22
(newline)

; based on this we can define cdr as

(define (cdr z)
  (define (iter x count divisor)
    (if (divides? divisor x)
      (iter (/ x divisor) (+ count 1) divisor)
      count))
  (iter z 0 3))

; and do similar tests
(display (cdr (cons 2 1))) ; expected 1
(newline)

(display (cdr (cons 3 2))) ; expected 2
(newline)

(display (cdr (cons 2 22))) ; expected 22
(newline)

; we can as well extract the iterative version of factor counting as factor counter as
(define (count-factors x divisor)
  (define (iter x count divisor)
    (if (divides? divisor x)
      (iter (/ x divisor) (+ count 1) divisor)
      count))
  (iter x 0 divisor))

; then we can redefine the car and cdr as
(define (car z)
  (count-factors z 2))

(define (cdr z)
  (count-factors z 3))

; and repeat the tests for both car and cdr
(display (car (cons 1 2))) ; expected 1
(newline)

(display (car (cons 2 3))) ; expected 2
(newline)

(display (car (cons 22 2))) ; expected 22
(newline)

(display (cdr (cons 2 1))) ; expected 1
(newline)

(display (cdr (cons 3 2))) ; expected 2
(newline)

(display (cdr (cons 2 22))) ; expected 22
(newline)

