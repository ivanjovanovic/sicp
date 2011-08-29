; One of the trivial problems which can describe the difference between
; recursive and iterative processes is calculation of factorial
; 
; n! = n*(n-1)*(n-2)*...*3*2*1
;
; This can be recursivelly defined and executed like this
;

(load "../common.scm")

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; (display (factorial 6)) ; 720
; (newline)

; By doing substitution we see the shape of the process
;
; (factorial 6)
; (* 6 (factorial 5))
; (* 6 (* 5 (factorial 4)))
; (* 6 (* 5 (* 4 (factorial 3))))
; (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
; (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
; (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
; (* 6 (* 5 (* 4 (* 3 2))))
; (* 6 (* 5 (* 4 6)))
; (* 6 (* 5 24))
; (* 6 120)
; 720
;
; We can see that representation of this process shows growth in two
; dimensions 
;
; Number of steps required to finish
; Number of elements that have to be saved for later execution
;
; For this process we see that if we add one more element we ahve to add
; two more steps so order of growth is O(2n) ~ O(n)
;
; Here we see as well that we need to save n number of elements for
; input of size n, so we say that it grows in space with O(n).
;
; For process with growth of 
; time  = O(n)
; space = O(n)
;
; we say it is `linear recursive`

; We can have as well different approach to calculating factorial

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter 
        (* product counter)
        (+ counter 1)
        max-count)))

(define (factorial n)
  (fact-iter 1 1 n))

; (display (factorial 6)) ; 720
; (newline)


; Shape of this process is completelly different if we do substitution
;
; (factorial 6)
; (fact-iter 1  1  6)
; (fact-iter 1  2  6)
; (fact-iter 2  3  6)
; (fact-iter 6  4  6)
; (fact-iter 24 5  6)
; (fact-iter 120 6 6)
; (fact-iter 720 7 6)
; 720

; here we see that process is linear in time = O(n) and space is
; constant since we do not have to remember any of the values for the
; next computation
;
; This kind of process is called `linear iteration`
;

; Tree recursion is one common pattern of recursive computation.
; Definition of fibonnaci sequence expains it
;

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
; (display (fib 5))
; (newline)

; One thing to see here is how for every call of fibonnaci we have 2
; more calls spawned during recursion. Here number of steps needed
; to calculate grow exponentially and space grows only linearly.
;
;                          fib 5
;                         /     \
;                       fib 4    fib 3 ....
;                     /     \
;                   fib 3    fib 2
;                 /     \       .....
;               fib 2   fib 1
;             /     \
;             fib 1  fib 0
;
; This part of the tree is presenting what is happening with the
; recursion. We can see that number of computations (width) is growing
; rapidly with the size of n while number of elements needed to be
; remembered (height) are growing slower and linear with size of the
; input.
;
; time(n) = Fib(n+1) ~ O(Phi^n)
; sace = O(n)
;
;
; Fibonacci sequence can be defined as well through a iterative process.

(define (fib n)
  (fib-iter 0 1 n))

(define (fib-iter a b count)
  (if (= count 0)
      a
      (fib-iter b (+ a b) (- count 1))))

; (display (fib 100))
; (newline)
;

; Calculating all the ways to return a change with the given list of
; money amounts is a bit more complicated recursive process.

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+
                (cc amount (- kinds-of-coins 1))
                (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; (display (count-change 11))
; (newline)
;
; This will take a while until it resolves to 292 ways to change 100
; with denominations of 50, 25, 10, 5 and 1


; 1.2.4 Exponantion
;
; Linear recursive process for exponantion of number to n-th exponent.
; b^n = b*b^(n-1), b^0 = 1

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; (display (expt 2 3))
; (newline)
;
; This can be implemented as linearly iterative process as well, with
; help of one more variable to transfer current state to next round
;

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b count product)
  (if (= count 0)
      product
      (expt-iter b (- count 1) (* product b))))


; (display (expt 2 32))
; (newline)
;
; This version is time-wise linear O(n) and space-wise constant O(1)
; But that means for calculating b^1000 it will take 1000 steps to
; calculate the number.
;
; We can do better than this with following algoithm which is based on
; general approach to half the size of the problem on every iteration.
;
; b^n = (b^(n/2))^2 - if n is even
; b^n = b * b^(n-1) - if n is odd
;

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


; (display (expt 2 33))
; (newline)

; 1.2.6 Testing for primality
;
; Searching for primary number naturally comes with the idea to find all
; divisors and then check of there are only two of them. Using some
; helper math which says, if n is not prime it must have a divisor
; less than or equal to n, we can write O(sqrt(n)) method for testing for primality.
;
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

; Another way to implement test for primality is by implementing
; Fermat's Little theorem which is
;
; Fermat's Little Theorem: If n is a prime number and a is any
; positive integer less than n, then a raised to the nth power is congruent to a modulo n.
;
; To implement the Fermat test, we need a procedure that computes the exponential of a number modulo another number:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; based on this Fermat test is

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

; (display (fermat-test 12323))
; (newline)

; This test is probabilistic and can be fooled so we can execute several
; times to prove it with different values for a.

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; (display (fast-prime? 1232312323 10))
; (newline)



