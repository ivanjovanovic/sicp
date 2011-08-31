; Exercise 1.33.
;
; You can obtain an even more general version of accumulate (exercise 1.32)
; by introducing the notion of a filter on the terms to be combined. That is,
; combine only those terms derived from values in the range that satisfy a
; specified condition. The resulting filtered-accumulate abstraction takes
; the same arguments as accumulate, together with an additional predicate of
; one argument that specifies the filter. Write filtered-accumulate as a procedure.
; Show how to express the following using filtered-accumulate:

; a. the sum of the squares of the prime numbers in the interval a to b
;    (assuming that you have a prime? predicate already written)

; b. the product of all the positive integers less than n that are relatively prime
;    to n (i.e., all positive integers i < n such that GCD(i,n) = 1).
;
; ----------------------------------------------------------------------------

(load "../1.2/1.2.scm")
(load "../1.2/1.20.scm")
(load "../common.scm")

(define (filtered-accumulate combiner null-value filter term a next b)
  (if (> a b)
    null-value
    (combiner 
      (if (filter a) (term a) null-value)
      (filtered-accumulate combiner null-value filter term (next a) next b))))

; Solution to a)
(define (next x) (+ x 1))
(define (sum-prime-squares a b)
    (filtered-accumulate + 0 prime? square a next b))

; (display (sum-prime-squares 1 10)) ; 88
; (newline)

; Solution to b)
(define (sum-relative-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 relative-prime? identity 1 next n))

; Didn't check if this is real result. A bit bored with repeating
; all of these similar examples
; (display (sum-relative-primes 10)) ; 189
; (newline)

