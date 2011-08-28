; TODO: run-test method runs a bit slowish.
;       If I change logical operator in iterations to "or" then it runs
;       faster. Maybe there is a way to optimize this or maybe to see how
;       heist implements this.
;
; Exercise 1.27. Demonstrate that the Carmichael numbers listed in footnote
; 47 (561, 1105, 1729, 2465, 2821, and 6601) really do fool the Fermat test.
; That is, write a procedure that takes an integer n and tests whether an is
; congruent to a modulo n for every a < n,and try your procedure on the given
; Carmichael numbers.
; ---------------------

(load "1.2.scm")

(define (pick-a-prime n)
  (cond
    ((= n 1) 561)
    ((= n 2) 1105)
    ((= n 3) 1729)
    ((= n 4) 2465)
    ((= n 5) 2821)
    ((= n 6) 6601)
    (else 1))
)

; for every given prime execute a test

(define (run-tests n)
  (cond ((= (pick-a-prime n) 1)
          (display "done")
          (newline))
        (else
          (test-cm-number (pick-a-prime n))
          (run-tests (- n 1)))))

(define (test-cm-number n)
  (cm-test-iteration 1 n true))

(define (cm-test-iteration a n is-it?)
  (cond ((= a n)
          (display n)
          (display ": ")
          (display is-it?)
          (newline))
        (else
          (cm-test-iteration (+ a 1) n (and is-it? (fermat-test a n))))))

(define (fermat-test a n)
  (= (expmod a n n) a))

; What we expect is that these numbers all be resolved as primes, although they are not.
; We can check that by running them through prime? method defined in 1.2.scm file.
; Fermat test gives only false positives. False negatives are not possible.

(run-tests 6)

; This will give a list of tested numbers with the result aside. As seen they are all claimed
; to be primes but no one of them is in fact. So Fermat test is proven to give false positives with
; Carmichael numbers from footnote 47.
