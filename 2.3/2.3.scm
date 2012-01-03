; Symbols are defined by quoting 

(load "../common.scm")
(load "../helpers.scm")

(define symbol-a 'a)

(define a 5)

; (display 'a) ; a
; (display a) ; 5

; operation eq? tests if symbols are the sam

; (display (eq? 'a 'a) ) ; true

; one useful function can be defined to return sublist of the list that
; starts with a given symbol

(define (memq symbol lst)
  (cond ((null? lst) false)
        ((eq? (car lst) symbol) lst)
        (else (memq symbol (cdr lst))))) ; here recursion just returns the value of the deepest recursion

; (display (memq 'a (list 'c 'a 'b 'd))) ; (a b d)

; To practice working with symbols we'll implement a program for symbolic differentiation which will
; based on reduction rules transform symbols to produce their derivatives
;
; before all we will need some procedures to explore elements of our symbolic data and build
; proper constructors and selectors around them
;
; (variable? v)
; (same-variable? v1 v2)
; (sum? e)
; (addend e)
; (augend e)
; (make-sum a1 a2)
; (product? e)
; (multiplier e)
; (multiplicand e)
; (make-product m1 m2)
;
; using these we can express derivative rules

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
        (else
          (error "unknown expression type" exp))))

; if we use original LISP prefix notation for the derivative symbols as well then we can define following procedures

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (list '+ a1 a2))

(define (make-product m1 m2)
  (list '* m1 m2))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (addend e) (cadr e))
(define (augend e) (caddr e))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

(define (multiplier e) (cadr e))
(define (multiplicand e) (caddr e))

; (output (deriv '(+ 2 x) 'x))

; In order to make ouput of the deriv a bit more compact we can
; redefine constructors for sum and product symbols a bit more complex

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

; is expression a number equal to something
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

; (output (deriv '(* x y (+ x 3)) 'x))


; Representing sets
;
; as unordered lists

; checking if element is part of the set

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; (output (element-of-set? 1 (list 1 2 3)))

; using this we can write adjoin-set

(define (adjoin-set x set)
  (if (not (element-of-set? x set))
    (append set (list x)) ; I like append more than cons here
    set)); just return set

; (output (adjoin-set 4 (list 1 2 3)))

; Intersection is defined as set that contains elements from both sets

(define (intersection-set set1 set2)
  (cond ((or (equal? set1 '()) (equal? set2 '())) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; (output (intersection-set (list 1 7 3 6 4 22) (list 1 2 3 4 22)))
