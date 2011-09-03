; Exercise 1.46.
;
; Several of the numerical methods described in this chapter are instances of an
; extremely general computational strategy known as iterative improvement. Iterative
; improvement says that, to compute something, we start with an initial guess for the
; answer, test if the guess is good enough, and otherwise improve the guess and continue
; the process using the improved guess as the new guess. Write a procedure iterative-improve
; that takes two procedures as arguments: a method for telling whether a guess is good enough
; and a method for improving a guess. Iterative-improve should return as its value a procedure
; that takes a guess as argument and keeps improving the guess until it is good enough.
; Rewrite the sqrt procedure of section 1.1.7 and the fixed-point procedure of
; section 1.3.3 in terms of iterative-improve.
;

(load "../common.scm")

; generic function of iterative improvement
(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (define (iter guess)
      ; we use new-guess on more than one place so place it in variable
      (let ((new-guess (improve-guess guess))) 
        (if (good-enough? new-guess)
          new-guess
          (iter new-guess))))
    (iter guess)))

; sqrt implementation
(define (sqrt-iter guess x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) guess))

; (display (sqrt-iter 1 64)) ; converges to 8
; (newline)

; fixed point. Since applying f continuously is in fact improving guess
; then we don't need separate improve function
(define (fixed-point-iter f guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) 0.001))
  ((iterative-improve close-enough? f) guess))

; (display (fixed-point-iter cos 1)) ; converges to 0.7395672022122561
; (newline)

