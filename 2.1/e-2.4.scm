; Exercise 2.4.  Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons x y)) yields x for any objects x and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the substitution model of section 1.1.5.)
; ------------------------------------------------------
;
; First, lets see if (car (cons x y)) returns x for any x

; first 1 and 2 are primitive ints
(display (car (cons 1 2))) ; 1
(newline)

; now we can try the lists, so we see that it is in fact procedure returned
(display (car (cons (cons 1 2) (cons 3 4))))
(newline)

; similarly we see that it doesn't matter what x and y are in fact. But they are perceived as
; list members and returned accordingly. Here we can see the power of having procedures together
; with primitives as first-class citizens.

; Secondly, we can define cdr as 

(define (cdr z)
  (z (lambda (p q) q)))

(display (cdr (cons 1 2))) ; 2
(newline)

; Substitution model for cdr would look like this.

(cdr (cons 1 2))
(cdr (lambda (m) (m 1 2))) ; evaluating argument of cdr
((lambda (m) (m 1 2)) (lambda (p q) q))) ; now evaluating cdr itself
((lambda (p q) q)) 1 2) ; applying operator to operand
((lambda (1 2) 2)) ; once again
; and this produces 2
