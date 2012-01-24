; Exercise 2.87.
;
; Install =zero? for polynomials in the generic
; arithmetic package. This will allow adjoin-term to work for
; polynomials with coefficients that are themselves polynomials.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "arithmetics.scm")
(load "symbolic_algebra.scm")

; polynoial is zero if all terms are zeor, we can cdr down the term list 
; to check all the terms

(define (update-polynomial-package)
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (zero-poly? poly)
   (define (zero-terms? termlist)
     (if (empty-termlist? termlist)
       '#t
       (and (=zero? (coeff (first-term termlist))) 
              (zero-terms? (rest-terms termlist)))))
   (zero-terms? (term-list poly)))
  (put '=zero? '(polynomial) zero-poly?))

(update-polynomial-package)

(output (=zero? (make-polynomial 'x (list (list 1 0) (list 0 0)) ))) ; #t
(output (=zero? (make-polynomial 'x (list (list 1 0) (list 0 1)) ))) ; #f
