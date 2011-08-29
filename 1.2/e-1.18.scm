; Exercise 1.18
;
; Using the results of exercises 1.16 and 1.17, devise a procedure that generates 
; an iterative process for multiplying two integers in terms of adding, doubling, 
; and halving and uses a logarithmic number of steps.
;

(load "../common.scm")
(load "e-1.16.scm")

; We can do this by implementing so called Russian peasants method of multiplication.
; This method is based on conversion of one of the multiplicators to binary form
; and then multiplying other with the powers of 2 for the places where binary form
; has 1 as the value.
;
; Example:
;
; 12 * 13
;
; Converting 12 to binary form:
;
; 12 / 2 = 6  with rest 0
; 6 / 2 = 3   with rest 0
; 3 / 2 = 1   with rest 1
; 1 / 2 = 0   with rest 1
;
; so this is 1100 in binary form and it is 2^3 + 2^2. Therefore, our solution is
; 2^3 * 13 + 2^2 * 13 = 156
;
; Converted to iterative algorithm that looks like this.
;
; I reuse expt function from the exercise 1.16 which is loaded

(define (mult a b)
  (fast-mult a b 0 0))

(define (fast-mult a b product counter)
  (cond ((= a 0) product)
        ((= (remainder a 2) 1) (fast-mult (div a 2) b (+ product (* (expt 2 counter) b)) (+ counter 1)))
        (else (fast-mult (div a 2) b product (+ counter 1)))))

(display (mult 120 13))
(newline)
;
; Here, instead of continuously adding a times we halve the size of the problem in every iteration.
; Therefore the number of steps of the algorithm grow logaritmicaly with the size of the problem.
