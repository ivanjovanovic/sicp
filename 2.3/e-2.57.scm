; Exercise 2.57.
;
; Extend the differentiation program to handle sums and
; products of arbitrary numbers of (two or more) terms. Then the last example
; above could be expressed as

; (deriv '(* x y (+ x 3)) 'x)

; Try to do this by changing only the representation for sums and products,
; without changing the deriv procedure at all. For example, the addend of a sum
; would be the first term, and the augend would be the sum of the rest of the
; terms.
; ------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.3.scm")

; first to see how are these procedures currently implemented

; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp)
;          (if (same-variable? exp var) 1 0))
;         ((sum? exp)
;          (make-sum (deriv (addend exp) var)
;                    (deriv (augend exp) var)))
;         ((product? exp)
;          (make-sum 
;            (make-product (multiplier exp) 
;                          (deriv (multiplicand exp) var))
;            (make-product (multiplicand exp)
;                          (deriv (multiplier exp) var))))
;         (else
;           (error "unknown expression type" exp))))


; (define (sum? e)
;   (and (pair? e) (eq? (car e) '+)))
; 
; (define (addend e) (cadr e))
; (define (augend e) (caddr e))
; 
; (define (product? e)
;   (and (pair? e) (eq? (car e) '*)))
; 
; (define (multiplier e) (cadr e))
; (define (multiplicand e) (caddr e))
;  
; (define (make-sum a1 a2)
;   (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;         (else (list '+ a1 a2))))
; 
; ; is expression a number equal to something
; (define (=number? exp num)
;   (and (number? exp) (= exp num)))
; 
; (define (make-product m1 m2)
;   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;         ((=number? m1 1) m2)
;         ((=number? m2 1) m1)
;         ((and (number? m1) (number? m2)) (* m1 m2))
;         (else (list '* m1 m2))))

(define (multiplicand e)
  (if (not (null? (cadddr e)))
    (cons '* (cddr e))
    (caddr e)))

(define (augend e)
  (if (not (null? (cadddr e)))
    (cons '+ (cddr e))
    (caddr e)))

; The way derivation is implemented we don't need to change
; sum and product constructors to get proper results. Although 
; they can be updated to make products and sums of multiple elements

; (output (multiplicand '(* 3 4 5)))
(output (deriv '(* x x (+ x 1)) 'x))


