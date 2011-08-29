; Exercise 1.24.
;
; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the Fermat method),
; and test each of the 12 primes you found in that exercise. Since the Fermat test has (log n)
; growth, how would you expect the time to test primes near 1,000,000 to compare with the time
; needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?

(load "../common.scm")
(load "1.2.scm")

; Copy paste from 1.22 to be reused here without interference
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

; reusing fast-prime? from 1.2.scm with 1 probe on Fermat test
(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
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
; 1000037 *** 590859.1747283936
; 1000033 *** 454282.9990386963
; 1000003 *** 447068.9296722412
; 100043 *** 407694.81658935547
; 100019 *** 402029.75273132324
; 100003 *** 393846.0350036621
; 10037 *** 364479.06494140625
; 10009 *** 349192.85774230957
; 10007 *** 361101.8657684326
; 1019 *** 296539.0682220459
; 1013 *** 286405.8017730713
; 1009 *** 268604.99382019043

; Numbers around 1000 are calculated almost with the same speed as in
; 1.22, number around 1.000.000 are calculated now much faster since
; O(log(n)) is much better for bigger size of the problem which is
; expected behaviour.
