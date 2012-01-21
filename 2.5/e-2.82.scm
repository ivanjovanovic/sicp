; Exercise 2.82.
;
; Show how to generalize apply-generic to handle coercion in
; the general case of multiple arguments. One strategy is to attempt to coerce
; all the arguments to the type of the first argument, then to the type of the
; second argument, and so on. Give an example of a situation where this
; strategy (and likewise the two-argument version given above) is not
; sufficiently general. (Hint: Consider the case where there are some suitable
; mixed-type operations present in the table that will not be tried.)
; ------------------------------------------------------------


(load "../helpers.scm")
(load "2.5.scm")
(load "e-2.78.scm")
(load "e-2.81")

; Old definition that works only on two arguments
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map content args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; more generic one that can handle arbitrary number of args


(define (apply-generic op . args)
  ; coercing list to a type
  (define (coerce-list-to-type lst type)
    (if (null? lst) 
      '()
      (let ((t1->t2 (get-coercion (type-tag (car lst)) type)))
        (if t1->t2
          (cons (t1->t2 (car lst)) (coerce-list-to-type (cdr lst) type))
          (cons (car lst) (coerce-list-to-type (cdr lst) type))))))

  ; applying to a list of multiple arguments
  (define (apply-coerced lst)
    (if (null? lst)
      (error "No method for given arguments")
      (let ((coerced-list (coerce-list-to-type args (type-tag (car lst)))))
        (let ((proc (get op (map type-tag coerced-list))))
          (if proc
            (apply proc (map content coerced-list))
            (apply-coerced (cdr lst)))))))

  ; logic to prevent always coercing if there is already direct input entry
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map content args))
        (apply-coerced args)))))

(output (apply-generic 'add 4 5))
(output (get-coercion 'complex 'complex))

; (output (coerce-list-to-type (list (make-complex-from-real-imag 3 4) (make-complex-from-real-imag 4 5)) 'complex))
; (output (coerce-list-to-type (list 4 3) 'complex))
; (output (get 'add (map type-tag (coerce-list-to-type (list 5 (make-complex-from-real-imag 3 4)) 'complex))))
(output (apply-generic 'add 5 (make-complex-from-real-imag 3 4)))

; Here, we need to have procedures that can handle this apply-generic which can work with multiple arguments.
; All our packages have installed only procedures with 2 arguments, and passing 3 arguments immediatelly provides error.
;
; One way is to update whole package but I guess in this exercise that is not the point.
