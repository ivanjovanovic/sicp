; Exercise 1.31
;
; a.  The sum procedure is only the simplest of a vast number of similar abstractions
;     that can be captured as higher-order procedures. Write an analogous procedure
;     called product that returns the product of the values of a function at points over
;     a given range. Show how to define factorial in terms of product. Also use product
;     to compute approximations to value of pi using the formula.
;
;     pi   2 * 4 * 4 * 6 * 6 * 8 ...
;     -- = -------------------------
;     4    3 * 3 * 5 * 5 * 7 * 7 ...
;
; b.  If your product procedure generates a recursive process, write one that generates
;     an iterative process. If it generates an iterative process, write one that generates
;     a recursive process.
;
;     ----------------------------------------------------------------------------------
;

(load "../common.scm")

; definition of the product
(define (product term a next b)
  (if (> a b)
      1
      (*
        (term a)
        (product term (next a) next b))))

(define (fact n)
  (define (identity x) x)
  (define (next x) (+ x 1))
  (product identity 1 next n))

; (display (fact 10)) ; 3628800
; (newline)

(define (pi n)
  (define (next x) (+ x 2))
  (* 4.0 (/ (* 2 (product square 4 next (- n 2)) n ) (product square 3 next n))))

; (display (pi 400)) ; approximates to 3.14
; (newline)

; iterative process definition
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

; Now we can execute approximation of pi with better precision since we use less memory
; (display (pi 4000)) ;
; (newline)
