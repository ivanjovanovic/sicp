; Since lot of exercises in this chapter ae building
; continuouusly up our arithmetic package, I use this place to put all
; our progress so it can be included easily in the next exercise.
; And fore easier reasoning about it.

(require-extension srfi-69) 

;; methods for attaching and resolving tags on data

(define (attach-tag type-tag contents)
  (if (number? contents)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else "bad tagged data - TYPE-TAG" datum)))
(define (content datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else "bad tagged data" datum)))

;;;; Operations hash table
(define op-table (make-hash-table))
(define (put op types val)
  (hash-table-set! op-table (list op types) val))
(define (get op types)
  (hash-table-ref/default op-table (list op types) #f))

;;;; Coercion table
(define coercion-table (make-hash-table))
(define (put-coercion type1 type2 proc)
  (hash-table-set! coercion-table (list type1 type2) proc))
(define (get-coercion type1 type2)
  (hash-table-ref/default coercion-table (list type1 type2) #f))

;; Coercion procedures
(define (scheme-number->complex n) (make-complex-from-real-imag (content n) 0))
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; type level procedures management
(define type-levels (make-hash-table))
(define (put-type-level type level)
  (hash-table-set! type-levels type level))
(define (get-type-level type)
  (hash-table-ref type-levels type))
(define (type-raisable? from to) (< (get-type-level from) (get-type-level to)))

(put-type-level 'scheme-number 0)
(put-type-level 'rational 1)
(put-type-level 'complex 2)

; is type down-pushable
(define (pushable? n)
  (and (> (get-type-level (type-tag n)) 0)
       (equ? n (raise (push-down n)))))

; drop type to lowes possible level
(define (drop n)
  (if (not (pushable? n))
    n
    (drop (push-down n))))

;;; Generic operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
(define (make-complex-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a) ((get 'make-from-mag-ang 'complex) r a))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (raise n) (apply-generic 'raise n))
(define (push-down n) (apply-generic 'push-down n))

;;; Scheme-number package

(define (make-scheme-number n) ((get 'make 'scheme-number) n))
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (raise-scheme-number n) (make-rational n 1))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) 
       equal?)
  (put '=zero? '(scheme-number) (lambda (x) (= 0 x)))
  (put 'raise '(scheme-number) raise-scheme-number)
  'done)

(install-scheme-number-package)

;;; Rational number arithmetics package
(define (make-rational n d) ((get 'make 'rational) n d))
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (raise-rational-to-complex n)
    (make-complex-from-real-imag (/ (numer n) (denom n)) 0))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y) (and (equal? (car x) (car y))
                          (equal? (cdr x) (cdr y)))))
  (put '=zero? '(rational)
       (lambda (x) (= 0 (car x))))
  (put 'raise '(rational) raise-rational-to-complex)
  (define (push-rational-down n)
    (round (/ (numer n) (denom n))))
  (put 'push-down '(rational) push-rational-down)
  'done)

(install-rational-package)


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

(install-rectangular-package)

;;; Complex number arithmetics package
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

(install-polar-package)

;;; Complex package ignorant of the underlying types
(define (install-complex-package)
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (c1 c2) (and (equal? (real-part c1) (real-part c2))
                            (equal? (imag-part c1) (imag-part c2)))))
  (put '=zero? '(complex)
       (lambda (c1) (and (= 0 (real-part c1)) (= 0 (imag-part c1)))))
  (define (push-complex-down z)
    (make-rational (real-part z) 1))
  (put 'push-down '(complex) push-complex-down)
  'done)

(install-complex-package)


;; Apply generic is kind of brain of our type system that knows about how to invoke proper procedure
;; on set of type arguments and to evaluate proper generic operations
(define (apply-generic op . args)
  (define (raise-type from to)
    (if (not (type-raisable? (type-tag from) to))
      from
      (raise-type (raise from) to)))
  (define (raise-list-to-type lst type)
    (if (null? lst)
      '()
      (cons (raise-type (car lst) type) (raise-list-to-type (cdr lst) type))))
  (define (get-highest-type lst)
    (define (get-highest-type-iter iter-lst type)
      (if (null? iter-lst)
        type
        (if (type-raisable? type (type-tag (car iter-lst)))
          (get-highest-type-iter (cdr iter-lst) (type-tag (car iter-lst)))
          (get-highest-type-iter (cdr iter-lst) type))))
    (get-highest-type-iter lst 'scheme-number))
  (define (apply-raised args)
    (let ((highest-type (get-highest-type args)))
      (let ((raised-list (raise-list-to-type args highest-type)))
        (let ((type-tags (map type-tag raised-list)))
          (let ((proc (get op type-tags)))
            (if proc
              (if (non-dropable-operation op)
                (apply proc (map content raised-list))
                (drop (apply proc (map content raised-list))))
              (error "Coercion by raising to highes type didn't find appropriate procedure")))))))
  ; operation whose result should not be lowered to a minimum level type
  (define (non-dropable-operation op)
    (or (equal? op 'raise) (equal? op 'push-down) (equal? op 'equ?) (equal? op '=zero?)))
  ; logic to prevent always coercing if there is already direct input entry
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        ;; this is ugly hack in order not to drop the result
        ;; of the elements that are not that generically supported
        (if (non-dropable-operation op)
          (apply proc (map content args))
          (drop (apply proc (map content args))))
        (apply-raised args)))))
