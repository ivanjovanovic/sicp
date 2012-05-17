; Exercise 4.13.
;
; Scheme allows us to create new bindings for variables by
; means of define, but provides no way to get rid of bindings. Implement for
; the evaluator a special form make-unbound! that removes the binding of a
; given symbol from the environment in which the make-unbound! expression is
; evaluated. This problem is not completely specified. For example, should we
; remove only the binding in the first frame of the environment? Complete the
; specification and justify any choices you make.
; ------------------------------------------------------------

(load "e-4.12.scm")

; Using the generic environment traversing functions we can implement easily imlement
; this operation.

; if there is var found in the first frame of the environment
; we will just relink lists
(define (make-unbound! var env)
  (define (found-action vars vals)
     (set-car! vars '())
     (set-car! vals '()))
  (env-traverse-and-find var found-action '() env))

;(define global-env (list (cons (list 'x 'y) (list 1 2))))
;(output global-env)
;(make-unbound! 'y global-env)
;(output global-env)

