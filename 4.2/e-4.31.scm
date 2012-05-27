; Exercise 4.31.
;
; The approach taken in this
; section is somewhat unpleasant, because it
; makes an incompatible change to Scheme. It
; might be nicer to implement lazy evaluation as
; an upward-compatible extension, that is, so
; that ordinary Scheme programs will work as
; before. We can do this by extending the syntax
; of procedure declarations to let the user
; control whether or not arguments are to be
; delayed. While we're at it, we may as well also
; give the user the choice between delaying with
; and without memoization. For example, the
; definition

;(define (f a (b lazy) c (d lazy-memo))
  ;...)

; would define f to be a procedure of four
; arguments, where the first and third arguments
; are evaluated when the procedure is called, the
; second argument is delayed, and the fourth
; argument is both delayed and memoized. Thus,
; ordinary procedure definitions will produce the
; same behavior as ordinary Scheme, while adding
; the lazy-memo declaration to each parameter of
; every compound procedure will produce the
; behavior of the lazy evaluator defined in this
; section. Design and implement the changes
; required to produce such an extension to
; Scheme. You will have to implement new syntax
; procedures to handle the new syntax for define.
; You must also arrange for eval or apply to
; determine when arguments are to be delayed, and
; to force or delay arguments accordingly, and
; you must arrange for forcing to memoize or not,
; as appropriate.
;
; ------------------------------------------------------------

; In this case I would reimplement procedure list-of-values
; to be able to differentiate value based on type.
;
; list of arguments to which procedure is applied
(define (list-of-values types exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (car types) (first-operand exps) env) ; changed
                (list-of-values (cdr types) (rest-operands exps) env))))

; actual value would be implemented to know how
; to deal with different types of the values
(define (actual-value type exp env)
  (case
    ((lazy-value? type) (force-lazy (eval exp env)))
    ((lazy-memo-value? type) (force-lazy-memo (eval exp env)))
    (else (eval exp env))))

; application procedure will differ just a bit to
; provide the list of types to the value resolver
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-values (procedure-param-types procedure) arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; we have to implement then these newly defined procedures

; 1. We would need to change (procedure-parameters) to return
; only parameters without additional type info in order
; to build environment properly

; 2. lazy-value?, lazy-memo-value?
;
; 3. We have already implementations of force-it without and
; with the memoization in 4.2.scm and we could use these.
