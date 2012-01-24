; Exercise 2.88.
;
; Extend the polynomial system to include subtraction of
; polynomials. (Hint: You may find it helpful to define a generic
; negation operation.)
; ------------------------------------------------------------

; substractions is in fact addition of the negation of the polynomial.
; So we'll define negation as negation of all the positive coefficients,
; and then just add polys together

; this one should be installed in all the arithmetical packages in order
; to work on other types beside poly

(load "../helpers.scm")
(load "arithmetics.scm")
(load "symbolic_algebra.scm")

(define (negate n) (apply-generic 'negate n))

;; This doesn't work if we have apply-generic which tries to drop the
;; result to the lowest terms. We have to add to polynomial package
;; tools for manipulating the type tower if we consider it as a super
;; type, or we can just use simpler apply-generic.
;;
;; Anyway implementation in perfect case would be like this

(define (update-polynomial-package)

  ;;; Imported just to make our package work
  (define (make-poly variable term-list)
    (cons variable term-list))
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
    (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys are not in the same variable --- ADD-POLY" (list p1 p2))))
  ; we used generic add for the coefficient arithmetics which will allow
  ; us on lot of interesting options
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))



  ;;; Our package update
  (define (negate-terms terms)
    (map (lambda (term) (list (order term) (* -1 (coeff term)))) terms))
  (define (negate-poly p)
    (make-poly (variable p) (negate-terms (term-list p))))
  (put 'negate '(polynomial) negate-poly)
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
      (add-poly p1 (negate p2))
      (error "Not same variable of polys -- SUB-POLY" (list p1 p2))))
  (put 'sub '(polynomial polynomial) sub-poly))

(update-polynomial-package)

(output (negate (make-polynomial 'x (list (list 0 1)))))
; (output (sub (make-polynomial 'x (list (list 0 2)))
;              (make-polynomial 'x (list (list 0 1)))))
