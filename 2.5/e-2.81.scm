(load "2.5.scm")
(load "e-2.78.scm") ; to use normal scheme numbers instead of typed

; Exercise 2.81.
;
; Louis Reasoner has noticed that apply-generic may try
; to coerce the arguments to each other's type even if they already have
; the same type. Therefore, he reasons, we need to put procedures in the
; coercion table to "coerce" arguments of each type to their own type.
; For example, in addition to the scheme-number->complex coercion shown
; above, he would do:

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

; a. With Louis's coercion procedures installed, what happens if
; apply-generic is called with two arguments of type scheme-number or
; two arguments of type complex for an operation that is not found in
; the table for those types? For example, assume that we've defined a
; generic exponentiation operation:

(define (exp x y) (apply-generic 'exp x y))

; and have put a procedure for exponentiation in the Scheme-number
; package but not in any other package:

;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (attach-tag 'exp (expt x y)))) ; using primitive expt

; What happens if we call exp with two complex numbers as arguments?

; b. Is Louis correct that something had to be done about coercion with
; arguments of the same type, or does apply-generic work correctly as
; is?

; c. Modify apply-generic so that it doesn't try coercion if the two
; arguments have the same type.
; ------------------------------------------------------------

; a) Lets see what happens when we call exp with two complex numbers.

(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 5 6))

; (output (exp 3 4)) ; since we installed it for scheme-number this works
; (output (exp z1 z2)) ; this will not work since we did not install it int the complex number package

; b) Lets see what is going on in the apply-generic and see what is wrong if someting
;    Then we can define what to do with it.

; Taking a look at the apply-generic procedure we see that at the bottom
; our condition doesn't really make sense, since both values of t1->t2 and t2->t1 are the same
; and semantically this is wrong. Although it will work correctly since it will always execute first condition
; but then it will do 3 steps more. It will always ask for coercion procedures, and it will always apply one,
; without any reason to do it.
;
; (define (apply-generic op . args)
;   (let ((type-tags (map type-tag args)))
;     (let ((proc (get op type-tags)))
;      (if proc
;           (apply proc (map content args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (let ((t1->t2 (get-coercion type1 type2))
;                      (t2->t1 (get-coercion type2 type1)))
;                  (cond (t1->t2
;                         (apply-generic op (t1->t2 a1) a2))
;                        (t2->t1
;                         (apply-generic op a1 (t2->t1 a2)))
;                        (else
;                         (error "No method for these types"
;                                (list op type-tags))))))
;              (error "No method for these types"
;                     (list op type-tags)))))))

; What se can do is to check before getting into condition if both types are the same and then just not to do 
; any coercion. So we redefine the apply-generic procedure

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
                ; here we add our check
                (if (eq? type1 type2)
                  (apply-generic op a1 a2) ; just apply generic without going into coercions
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
                     (list op type-tags))))))))

; (output (exp 3 4))
