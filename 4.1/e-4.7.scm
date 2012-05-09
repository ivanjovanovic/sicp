; Exercise 4.7.  Let* is similar to let, except that the bindings of the let
; variables are performed sequentially from left to right, and each binding is
; made in an environment in which all of the preceding bindings are visible.
; For example

; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))

; returns 39. Explain how a let* expression can be rewritten as a set of nested
; let expressions, and write a procedure let*->nested-lets that performs this
; transformation. If we have already implemented let (exercise 4.6) and we want
; to extend the evaluator to handle let*, is it sufficient to add a clause to
; eval whose action is

; (eval (let*->nested-lets exp) env)

; or must we explicitly expand let* in terms of non-derived expressions?
; ------------------------------------------------------------

(load "../helpers.scm")
(load "e-4.6.scm")

(define (let*->nested-lets exp)
  (expand-let* (cadr exp) (caddr exp)))

(define (expand-let* bindings body)
  (if (null? (cdr bindings))
    (make-let (car bindings) body)
    (make-let (car bindings) (expand-let* (cdr bindings) body))))

(define (make-let bindings body)
  (list 'let (list bindings) body))

(output (let*->nested-lets '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))))
; outputs (let ((x 3)) (let ((y (+ x 2))) (let ((z (+ x y 5))) (* x z))))

; I think using it as (eval (let*->nested-lets exp) env) should be safe
; because lets are transformed to application of (lambdas) which evaluate with changing the
; environments as needed, so setting new variable in the environment would be as expected
