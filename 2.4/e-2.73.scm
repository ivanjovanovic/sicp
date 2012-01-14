; Exercise 2.73.
;
; Section 2.3.2 described a program that performs
; symbolic differentiation:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ; <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

; We can regard this program as performing a dispatch on the type of the
; expression to be differentiated. In this situation the ``type tag'' of the
; datum is the algebraic operator symbol (such as +) and the operation being
; performed is deriv. We can transform this program into data-directed style by
; rewriting the basic derivative procedure as

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a.  Explain what was done above. Why can't we assimilate the predicates
; number? and same-variable? into the data-directed dispatch?

; b.  Write the procedures for derivatives of sums and products, and the
; auxiliary code required to install them in the table used by the program
; above.

; c.  Choose any additional differentiation rule that you like, such as the one
; for exponents (exercise 2.56), and install it in this data-directed system.

; d.  In this simple algebraic manipulator the type of an expression is the
; algebraic operator that binds it together. Suppose, however, we indexed the
; procedures in the opposite way, so that the dispatch line in deriv looked
; like

((get (operator exp) 'deriv) (operands exp) var)

; What corresponding changes to the derivative system are required?
; ------------------------------------------------------------

; a)
; 
; What we did in the given example is that we have filled the table with the procedures
; for every type of expression and then in the derivation process we load the operation to be executed
; and applying it to the operands. This way we can extend derivation rules by just adding entries
; to a generic table
;
; b) 


; has to be defined outside so it is visible to the product derivation package
; or it can be defined there as well as local, which is probably worse way of doing it.
(define (make-sum a b)
  (list '+ a b))
(define (make-product m1 m2)
  (list '* m1 m2))

(define install-sum-derivation
  (define (addend operands)
    (car operands))
  (define (augend operands)
    (cadr operands))
  (define (derive-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  ; and methods for putting the thing in the table
  (put '+ 'deriv derive-sum))

(define install-product-derivation
  (define (multiplier operands)
    (car operands))
  (define (multiplicand operands)
    (cadr operands))
  (define (derive-product operands var)
    (make-sum
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))
      (make-product (multiplicand operands)
                    (deriv (multiplier operands) var))
      var))
  ; put that into table
  (put '* 'deriv derive-product))


; c)
;
; If we want to install new derivative rule, for example for exponents.
(define install-exponent-derivation
  (define (power operands)
    (cadr operands))
  (define (base operands)
    (car operands))
  (define (make-exponent b p)
    (list '** b p))
  (define (derive-exponent operands var)
    (make-product
      (make-product (power operands)
                    (make-exponent (base operands) (- (power operands) 1)))
      (deriv (base operands) var)))
  (put '** 'deriv derive-exponent))

; d)
;
; Nothing needs to change in the derivation process except the line which is already given as changed.
; The data abstraction to the table representation has to be made more coherent so put places data in table correctly as well.
; Therefore we would need to change the order of the put parameters.


; I won't be trying to test if this works since it is not the goal of the exercise to make working derivative system but to 
; understand the way we abstract "dispatch by type" mechanism into a generic table and how we use it in the practical application
; of derivation.
