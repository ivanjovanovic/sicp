; Exercise 3.35.  Ben Bitdiddle tells Louis that one way to avoid the
; trouble in exercise 3.34 is to define a squarer as a new primitive
; constraint. Fill in the missing portions in Ben's outline for a
; procedure to implement such a constraint:

; (define (squarer a b)
;   (define (process-new-value)
;     (if (has-value? b)
;         (if (< (get-value b) 0)
;             (error "square less than 0 -- SQUARER" (get-value b))
;             <alternative1>)
;         <alternative2>))
;   (define (process-forget-value) <body1>)
;   (define (me request) <body2>)
;   <rest of definition>
;   me)
; ------------------------------------------------------------

(load "../helpers.scm")
(load "constraints.scm")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))))

  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))
(squarer a b)

(set-value! a 100 'ivan)
(forget-value! a 'ivan)
(set-value! a 10 'ivan)
(output (get-value b))
(output (get-value a)) ; outputs #f which is wrong
