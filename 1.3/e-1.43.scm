; Exercise 1.43.
;
; If f is a numerical function and n is a positive integer, then we can form
; the nth repeated application of f, which is defined to be the function whose
; value at x is f(f(...(f(x))...)). For example, if f is the function x ->  x + 1,
; then the nth repeated application of f is the function x ->  x + n. If f is the
; operation of squaring a number, then the nth repeated application of f is the function
; that raises its argument to the 2nth power. Write a procedure that takes as inputs a
; procedure that computes f and a positive integer n and returns the procedure that
; computes the nth repeated application of f. Your procedure should be able to be used as follows:
;
; ((repeated square 2) 5) and to produce 625 as result

(load "../common.scm")

; Implementation of repeated application in linearly iterative fashion
(define (repeated f n)
  (lambda (x)
    (define (iter i result)
      (if (= i n)
        result
        (iter (inc i) (f result))))
    (iter 1 (f x))))


; (display ((repeated square 2) 5)) ; gives result 625 as expected
; (newline)

