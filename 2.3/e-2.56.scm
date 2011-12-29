; Exercise 2.56.
;
; Show how to extend the basic differentiator to handle more kinds of
; expressions. For instance, implement the differentiation rule
;
; d(u^n)/dx = n*u^(n-1)*du/dx

; by adding a new clause to the deriv program and defining appropriate
; procedures exponentiation?, base, exponent, and make-exponentiation. (You may
; use the symbol ** to denote exponentiation.) Build in the rules that anything
; raised to the power 0 is 1 and anything raised to the power 1 is the thing
; itself.
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.3.scm")

; augmented deriv function

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum 
           (make-product (multiplier exp) 
                         (deriv (multiplicand exp) var))
           (make-product (multiplicand exp)
                         (deriv (multiplier exp) var))))
        ((exponent? exp)
         (make-product 
           (make-product (power exp)
                         (make-exponent (base exp) (- (power exp) 1)))
           (deriv (base exp) var)))
        (else
          (error "unknown expression type" exp))))

(define (exponent? exp)
  (eq? (car exp) '**))

(define (make-exponent b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        (else (list '** b p))))

(define (base exp) (cadr exp))
(define (power exp) (caddr exp))


(output (deriv '(** (* x 3) 5) 'x))
