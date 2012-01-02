; Exercise 2.58.
;
; Suppose we want to modify the differentiation program so that
; it works with ordinary mathematical notation, in which + and * are infix
; rather than prefix operators. Since the differentiation program is defined in
; terms of abstract data, we can modify it to work with different
; representations of expressions solely by changing the predicates, selectors,
; and constructors that define the representation of the algebraic expressions
; on which the differentiator is to operate.

; a. Show how to do this in order to differentiate algebraic expressions
; presented in infix form, such as (x + (3 * (x + (y + 2)))). To simplify the
; task, assume that + and * always take two arguments and that expressions are
; fully parenthesized.

; b. The problem becomes substantially harder if we allow standard algebraic
; notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses
; and assumes that multiplication is done before addition. Can you design
; appropriate predicates, selectors, and constructors for this notation such
; that our derivative program still works?
;------------------------------------------------------------

(load "../common.scm")
(load "../helpers.scm")
(load "2.3.scm")

; First the easier solution for infix with the given constraint.
; In this case we have kind of restricted form of the infix which we
; can easily divide into elements without any questioning on the structure.

(define (sum? e)
  (and (pair? e) (eq? (cadr e) '+)))

(define (addend e) (car e))
(define (augend e) (caddr e))

(define (product? e)
  (and (pair? e) (eq? (cadr e) '*)))

(define (multiplier e) (car e))
(define (multiplicand e) (caddr e))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

; is expression a number equal to something
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; (output (deriv '(x + (3 * (x + (x + 2)))) 'x))
; (output (deriv '(x * (x * (x * (x * 2)))) 'x))

; b. More complicated case is when infix expression can have more than 2 elements. It should
; be able to derive the standard algebric notation  (x + 3 * (x + y + 2))

; with these two simple changes we get the right results without changing the
; deriv procedure. Here we have to check what is behind the infix operator and to decide 
; what t return as second tailing element of the expression, is it primitive or is it
; another subexpression. THe way deriv procedure is implemented is RD (recursive descendend)
; parser, so it will recurse through all the subexpressions to derive them properly

(define (augend e)
  (if (not (null? (cadddr e)))
    (cddr e)
    (caddr e)))

(define (multiplicand e) 
  (if (not (null? (cadddr e)))
    (cddr e)
    (caddr e)))

(output (deriv '(x + 3 * (x + y + 2)) 'x))
; (output (deriv '(x + x + x) 'x))
; (output (deriv '(x * 2 * 3) 'x))
