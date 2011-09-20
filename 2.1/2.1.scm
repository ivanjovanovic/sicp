; In the terms of wishful thinking, we can implement usage of rational
; numbers without even knowing the details of their implementation. Only
; thing we have to know is how to create them (make-rat x y), how to get
; numerator (numer r) and denominator (denom r). We don't have to know
; anything about their internals.

(load "../common.scm")

(define (add-rat x y)
  (make-rat
    (+ (* (numer x) (denom y))
       (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat
    (- (numer x) (denom y)
       (numer y) (denom x))
    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat 
    (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; We can implement now assumed functions in terms of pairs

; constructor
(define (make-rat n d) (cons n d))

; numerator selector
(define (numer x) (car x))

; denominator selector
(define (denom x) (cdr x))

; textual representation
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))
; (print-rat one-half)

(define one-third (make-rat 1 3))

; here we see it is not normalized to 2/3 but it shows 6/9
; (print-rat (add-rat one-third one-third))

; we can fix normalization by reimplementing make-rat with
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; here we see it is not normalized to 2/3 but it shows 6/9
; In heist it represents 2/1/3/1 which is not normalized on the other
; side but is correct mathematically.
; (print-rat (add-rat one-third one-third))

; one place where we can see that using the abstract representation of
; the data is hiding the internal details of the implementation is in
; the case where we want to change the implementation of the
; normalization of rational numbers to

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

; now instead in make-rat, we do normalization in selectors. There can
; be ratio behind this decision if we create numbers much more than
; selecting them.

; Taking the definition of data from notes.md we can show that pairs
; concept is implemendet without data at all, only with procedures.

; So pairs data structure is made purely of procedures
(define (cons x y)
  (lambda (z)
    (cond ((= z 1) x)
          ((= z 2) y))))

(define (car p) (p 1))
(define (cdr p) (p 2))


; Interval arithmetic from the section 2.1.4 can be implemented in its basic representation in the 
; following form with having upper and lower bound of the interval as pair. Consider for now that we
; have constructor and selectors as predefined
; http://en.wikipedia.org/wiki/Interval_arithmetic

(define (add-interval x y)
  (make-interval 
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

; multiplication are all combinations of products of bounds with new interval
; by picking lowest and highest of product combinations
(define (mul-interval x y)
  (let 
    ((p1 (* (lower-bound x) (lower-bound y)))
     (p2 (* (lower-bound x) (upper-bound y)))
     (p3 (* (upper-bound x) (lower-bound y)))
     (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval 
      (min p1 p2 p3 p4)
      (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (lower-bound y))
                   (/ 1.0 (upper-bound y)))))
