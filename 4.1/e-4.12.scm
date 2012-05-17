; Exercise 4.12.
;
; The procedures set-variable-value!, define-variable!, and
; lookup-variable-value can be expressed in terms of more abstract procedures
; for traversing the environment structure. Define abstractions that capture
; the common patterns and redefine the three procedures in terms of these
; abstractions.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "4.1.scm")

; we can define generic loop through along the environments and
; pass two procedures that will define what happens when variable
; is found and what when it is not

(define (env-traverse-and-find var found-proc not-found-proc env)
  (define (scan vars vals)
      (cond ((null? vars)
             (not-found-proc env))
            ((eq? var (car vars))
             (found-proc vars vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))


; One type of these loops is the one that goes through all of the enclosing environments
; and does some action if found. I can make more specific loop like this by providing the action
; explicitly
(define (env-deep-traverse-and-find var found-proc env)
  (define (continue-env-traversing-action env)
    (env-traverse-and-find var found-proc continue-env-traversing-action (enclosing-environment env)))
  (env-traverse-and-find var found-proc continue-env-traversing-action env))



; using these two we can reimplement the following
(define (set-variable-value! var val env)
  (define (found-action vars vals)
    (set-car! vals val))
  (env-deep-traverse-and-find var found-action env))
;
(define (lookup-variable-value var env)
  (define (found-action vars vals)
    (car vals))
  (env-deep-traverse-and-find var found-action env))
;
(define (define-variable! var val env)
  (define (not-found-action env)
    (add-binding-to-the-frame! var val (first-frame env)))
  (define (found-action vars vals)
    (set-car! vals val))
  (env-traverse-and-find var found-action not-found-action env))

;(define global-env (list (cons (list 'x 'y) (list 1 2))))
;(output (lookup-variable-value 'y global-env))

;(set-variable-value! 'x 5 global-env)
;(set-variable-value! 'y 6 global-env)
;(output (lookup-variable-value 'x global-env))
;(output (lookup-variable-value 'y global-env))

;(define-variable! 'z 10 global-env)
;(output (lookup-variable-value 'z global-env))
