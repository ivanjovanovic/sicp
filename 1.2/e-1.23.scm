; Exercise 1.23.
;
; The smallest-divisor procedure shown at the start of this section does lots of needless testing:
; After it checks to see if the number is divisible by 2 there is no point in checking to see if it
; is divisible by any larger even numbers. This suggests that the values used for test-divisor should
; not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7, 9, .... To implement this change, define a procedure
; next that returns 3 if its input is equal to 2 and otherwise returns its input plus 2.
; Modify the smallest-divisor procedure to use (next test-divisor) instead of (+ test-divisor 1).
; With timed-prime-test incorporating this modified version of smallest-divisor,
; run the test for each of the 12 primes found in exercise 1.22. Since this modification halves the
; number of test steps, you should expect it to run about twice as fast.
; Is this expectation confirmed? If not, what is the observed ratio of the speeds of the two algorithms,
; and how do you explain the fact that it is different from 2?
;

(load "common.scm")
(load "1.2.scm")

; define method to pick only odd ones
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

; use (next n) in here
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (prime? n)
  (= n (smallest-divisor n)))

; Copy paste from 1.22 to be reused here without interference
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

; Since one idea is not to introduce new syntax not mentioned in the
; previous sections I have to define list of primes to check
;
(define (pick-a-prime n)
  (cond
    ((= n 1) 1009)
    ((= n 2) 1013)
    ((= n 3) 1019)
    ((= n 4) 10007)
    ((= n 5) 10009)
    ((= n 6) 10037)
    ((= n 7) 100003)
    ((= n 8) 100019)
    ((= n 9) 100043)
    ((= n 10) 1000003)
    ((= n 11) 1000033)
    ((= n 12) 1000037)
    (else 2))
)

; To recall the times for the numbers in 1.22
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

; If I run

(define (run-primes n)
  (cond ((= n 0) (newline) (display "done") (newline))
        (else (timed-prime-test (pick-a-prime n))
              (run-primes (- n 1)))))

(run-primes 12)

; I get results
;
; 1000037 *** 7033523.082733154
; 1000033 *** 6983025.074005127
; 1000003 *** 6982669.115066528
; 100043 *** 2170840.9786224365
; 100019 *** 2309645.891189575
; 100003 *** 2180263.2808685303
; 10037 *** 697196.7220306396
; 10009 *** 718861.1030578613
; 10007 *** 712119.1024780273
; 1019 *** 242056.13136291504
; 1013 *** 224329.94842529297
; 1009 *** 242685.79483032227
;
; Heist interpreter is written in Ruby and I can imagine that the way
; it is implemented adds lot of weight to function calls compared to
; primitive operations so introduction of another function call adds
; some time.
