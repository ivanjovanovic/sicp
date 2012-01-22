; Exercise 2.85.
;
; This section mentioned a method for ``simplifying'' a
; data object by lowering it in the tower of types as far as possible.
; Design a procedure drop that accomplishes this for the tower described
; in exercise 2.83. The key is to decide, in some general way, whether
; an object can be lowered. For example, the complex number 1.5 + 0i can
; be lowered as far as real, the complex number 1 + 0i can be lowered as
; far as integer, and the complex number 2 + 3i cannot be lowered at
; all. Here is a plan for determining whether an object can be lowered:
; Begin by defining a generic operation project that ``pushes'' an
; object down in the tower. For example, projecting a complex number
; would involve throwing away the imaginary part. Then a number can be
; dropped if, when we project it and raise the result back to the type
; we started with, we end up with something equal to what we started
; with. Show how to implement this idea in detail, by writing a drop
; procedure that drops an object as far as possible. You will need to
; design the various projection operations53 and install project as a
; generic operation in the system. You will also need to make use of a
; generic equality predicate, such as described in exercise 2.79.
; Finally, use drop to rewrite apply-generic from exercise 2.84 so that
; it ``simplifies'' its answers.

(load "../helpers.scm")
(load "arithmetics.scm")

; define generic push down type
(define (push-down n) (apply-generic 'push-down n))

; for every package that has possibility to push down we implement
; concrete procedures

(define (update-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (push-rational-down n)
    (round (/ (numer n) (denom n))))
  (put 'push-down '(rational) push-rational-down))

(update-rational-package)

(define (update-complex-package)
  (define (push-complex-down z)
    (make-rational (real-part z) 1))
  (put 'push-down '(complex) push-complex-down))

(update-complex-package)

;; tests
; (output (push-down (make-rational 5 2)))
; (output (push-down (make-complex-from-real-imag 5 2)))

;; we have to design predicate to see if one number is pushable
(define (pushable? n)
  (and (> (get-type-level (type-tag n)) 0)
       (equ? n (raise (push-down n)))))

; (output (pushable? (make-rational 5 1))); #t
; (output (pushable? (make-rational 5 2))); #f

; should push the number furthest possible
(define (drop n)
  (if (not (pushable? n))
    n
    (drop (push-down n))))

; (output (drop (make-rational 4 2))) ; 2
; (output (drop (make-rational 4 3))) ; (rational 4 . 3)
; (output (drop (make-complex-from-real-imag 5 0))) ; (rational 4 . 3)
; (output (drop (make-complex-from-real-imag 5 1))) ; (complex rectangular 5 1)


; At the end we use new knowledge to drop the result of the apply-generic

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
              (drop (apply proc (map content raised-list)))
              (error "Coercion by raising to highes type didn't find appropriate procedure")))))))
  
  ; logic to prevent always coercing if there is already direct input entry
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        ;; this is ugly hack in order not to drop the result
        ;; of the elements that are not that generically supported
        (if (or (equal? op 'raise) (equal? op 'push-down) (equal? op 'equ?))
          (apply proc (map content args))
          (drop (apply proc (map content args))))
        (apply-raised args)))))

; (output (add 5 3)) 
; (output (add 3 (make-rational 5 1)))

; (output (drop (add 3 5)))
; (output (drop (add 3 (make-rational 5 1))))
; (push-down (make-rational 4 1))
; (output (pushable? (make-rational 4 2)))
(output (add (make-rational 4 1) (make-rational 4 1)))
; (output )
(output (push-down (make-rational 4 1)))


