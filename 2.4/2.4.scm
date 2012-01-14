; Old way definitions of the data abstractions around complex numbers.
;
; As usual, we start by wishful thinking in the higher level by defining
; the way we would use these abstractions and then implementing them in
; the terms of the lower language.
;
; Since our goal is to implement generic arithmetic for the complex
; numbers we define following procedures.


(define (add-complex z1 z2)
  (make-from-rect (+ (real-part z1) (real-part z2))
                  (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-rect (- (real z1) (real z2))
                  (- (imag z1) (imag z2))))

(define (mul-complex z1 z2)
  (make-from-polar (* (magnitude z1) (magnitude z2))
                   (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-polar (/ (magnitude z1) (magnitude z2))
                   (- (angle z1) (angle z2))))

; now is left to implement the constructors and selectors for the
; defined abstractions used
;

; Ben is defining them in the terms of pairs containing rectangular data

(define (make-from-rect x y)
  (cons x y))
(define (make-from-polar r a)
  (cons (* r (cos a)) (* r (sin a))))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))

; Alyssa, from other side decides to represent them in polar form as a
; pair of radius and angle. Therefore she comes with a different
; method implementations.

(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-rect x y) 
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-polar r a) (cons r a))

; By defining these abstractions we have ensured that arithmetics on
; complex numbers will work with both representations.
;
; Having two data abstractions that behave the same to the outside world
; enables us to defer the decision of the representation we want to use
; to the later moment.
;
; But, what if we make decision to have them both in the system. Then we
; still have to distinguish them somehow.
; We do this by introducing the type tag to a representation of the
; number, so it can be asked of which type it is.

; We define the data abstraction for the tagged data which is enriched
; data abstraction of the data
;
; constructor for the tagged contents
(define (attach-tag type-tag contents)
  (cons type-tag contents))

; selectors
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "bad tagged data -- TYPE-TAG" datum)))

(define (content datum)
  (if (pair? datum)
    (cdr datum)
    (error "bad tagged data -- CONTENTS" datum)))

; and some predicates

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


; Now if developer modify their code to use tagged data.
;
; For rectangular representation

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a) 
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; and for polar

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y) 
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


; now it is only left to make another level of abstraction that will
; enable us to be oblivious of the type we are working with.
;

(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

; This methodology is called dispatch by type.

; It is in fact just choosing which selector to call according to a type
; provided.
;
; What if we had third representation of the complex numbers. Then we
; would have to change the dispatch on type procedure to incorporate it. 
; Instead of that we can make a generic approach of selecting apropriate
; procedure for execution from a table.
; The abstraction of the table will give us (put) and (get) procedures
; to fill and retrieve the operations. We can define these later.
;
; With this in mind we can define the procedure to install the package
; in our system.
;
; We are defining internal procedures and just puting references to them
; in the table. Outer world doesn't have to know for them at all.

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


; Alyssa's polar package is analogous:

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; Now we have to define a way to apply provided operation to a list of
; arguments that are provided
(define (apply-generic op . args) ; taking arbitrary number of arguments
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types - APPLY-GENERIC"
          (list op type-tags))))))

; now in these generic terms we can refactor the rest of the system

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

; also we can put in the table which constructor to use
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
