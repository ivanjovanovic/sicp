; Exercise 1.45.
;
; We saw in section 1.3.3 that attempting to compute square roots by naively finding a
; fixed point of y -> x/y does not converge, and that this can be fixed by average damping.
; The same method works for finding cube roots as fixed points of the average-damped y -> x/y^2.
; Unfortunately, the process does not work for fourth roots -- a single average damp is not
; enough to make a fixed-point search for y -> x/y3 converge. On the other hand, if we
; average damp twice (i.e., use the average damp of the average damp of y -> x/y3) the fixed-point
; search does converge. Do some experiments to determine how many average damps are required to
; compute nth roots as a fixed-point search based upon repeated average damping of y -> x/yn-1.
; Use this to implement a simple procedure for computing nth roots using fixed-point, average-damp,
; and the repeated procedure of exercise 1.43. Assume that any arithmetic operations you need
; are available as primitives.
;
; -------------------------

; First lets repeat average dumping for squares
(load "../1.2/1.2.scm") ; for fast exponent procedure
(load "1.3.scm")
(load "e-1.36.scm") ; loading verbose fixed-point definition
(load "e-1.43.scm") ; loading procedure repeated

(define (sqrt x)
  (fixed-point (average-dump (lambda (y) (/ x y))) 1.0 0.000001))

; for square it converges
; (display (sqrt 5))
; (newline)

(define (fourth-root x)
  (fixed-point (average-dump (lambda (y) (/ x (cube y)))) 1.0 0.0001))

; this doesn't converge with only one avergae damping
; (display (fourth-root 100))
; (newline)

; So if we define general function for the n-th root of the number with
; configurable damps
(define (nth-root x n damps)
  (fixed-point ((repeated average-dump damps) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0 0.001))

; Lets experiment

; 2 average dampings
; (nth-root 32 5 2) ; converges to ~ 2
; (nth-root 64 6 2) ; converges to ~ 2
; (nth-root 128 7 2) ; converges to ~ 2
; (nth-root 256 8 2) ; doesn't converge

; (nth-root 256 8 3) ; converges to ~ 2
; .
; .
; .
; (nth-root 32768 15 3) ; converges to ~ 2
; (nth-root 32768 16 3) ; doesn't converge, oscillates around 1.9

; so, we can make small table of values
;
; damps  1 | 2 | 3
; ------------------
; roots  3 | 7 | 15
;
; If n is number of root and d number of damps then we can express this
; table as 
;
; n = 2^(d+1) - 1 =>  n + 1 = 2^(d+1) => log2(n + 1) = d + 1
; => d = log2(n + 1) - 1
;
; Here we can move this logarithm of base 2 to natural base e with help
; of identity logb(x) = logk(x)/logk(b)
; @see http://en.wikipedia.org/wiki/Logarithm
;
; therefore last form of our equation is 
;
; d = log(n + 1)/log(2) - 1
;
; with this, we can calculate how many number of damps to use for given
; n
;
(define (gimme-damps n)
  (ceiling (- (/ (log (+ n 1)) (log 2)) 1)))

; Redefined with calculated number of damps
(define (nth-root x n)
  (fixed-point ((repeated average-dump (gimme-damps n)) (lambda (y) (/ x (fast-expt y (- n 1))))) 1.0 0.001))

; test for an arbitrary combination of number and root level
; (nth-root 4294967296 32) ; converges to ~ 2, although pretty slow

; this one would take a lot of time but would converge also
; (nth-root 340282366920938463463374607431768211456 128)


