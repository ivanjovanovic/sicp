; Exercise 1.22.
;
; Most Lisp implementations include a primitive called runtime that returns an
; integer that specifies the amount of time the system has been running
; (measured, for example, in microseconds). The following timed-prime-test procedure,
; when called with an integer n, prints n and checks to see if n is prime.
;
; If n is prime, the procedure prints three asterisks followed by the amount of time used in performing the test.

(load "1.2.scm")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))


; Using this procedure, write a procedure search-for-primes that checks the primality of
; consecutive odd integers in a specified range.
; Use your procedure to find the three smallest primes larger than: 1000, 10,000, 100,000 and 1,000,000.
;
; Note the time needed to test each prime. Since the testing algorithm has order of growth of O(n),
; you should expect that testing for primes around 10,000 should take about sqrt(10) times as long as
; testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000
; and 1,000,000 support the n prediction?
; Is your result compatible with the notion that programs on your machine run in time proportional
; to the number of steps required for the computation?
; -------------------------------------------------------

; Implementation for searching within given range
(define (search-for-primes from to)
  (cond ((>= from to) (newline) (display "done") (newline))
        ((even? from) (search-for-primes (+ from 1) to)) ; just skip to the next odd
        (else (timed-prime-test from)
              (search-for-primes (+ from 2) to)))) ; take only odds

(define (>= a b)
  (or (= a b) (> a b)))

(search-for-primes 1000 1100)
;
; This produces the list of checked and found primes.
;
; Next three of, with calculation times:
;
; 1009 *** 270644.9031829834
; 1013 *** 276235.10360717773
; 1019 *** 267241.0011291504
;
; 10.000
; --------------
; 10007 *** 815484.7621917725
; 10009 *** 817969.0837860;
; 10037 *** 801926.851272583
;
; 100.000
; --------------
; 100003 *** 2640488.1477355957
; 100019 *** 2641607.9998016357
; 100043 *** 2636064.052581787
;
; 1.000.000
; --------------
;
; 1000003 *** 8373789.7872924805
; 1000033 *** 8346530.199050903
; 1000037 *** 8372905.015945435
;
; Algorithm we use here is of O(sqrt(n)) order so is assumable that for
; 10.000 it runs sqrt(10) times longer than for 1000.
;
; (sqrt 10) = 3.1622776601683795
;
; If we take the averages and divide them
; (8373789 + 8346530 + 8372905) / 3 = 8364408
; (2640488 + 2641607 + 2636064) / 3 = 2639386
; (815484 + 817969 + 801926) / 3 = 811793
; (270644 + 276235 + 267241) / 3 = 271373
; 
; (/ 811793.0  271373.0) = 2.9914287714695273
; (/ 2639386.0 811793.0) = 3.2513042117879802
; (/ 8364408.0 2639386.0) = 3.169073413286272
;
; Data shows that our assumption is in general correct. Although,
; the precision is better for the comparison of 100.000 and 1.000.000.
; I think it is because averaging is much better for bigger numbers
; since it takes more steps to perform and then converges to sqrt(n1/n2)
; better.
;
