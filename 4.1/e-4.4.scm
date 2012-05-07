; Exercise 4.4.
;
; Recall the definitions of the special forms and and or from
; chapter 1:

; and: The expressions are evaluated from left to right. If any expression
; evaluates to false, false is returned; any remaining expressions are not
; evaluated. If all the expressions evaluate to true values, the value of the
; last expression is returned. If there are no expressions then true is
; returned.
;
; or: The expressions are evaluated from left to right. If any
; expression evaluates to a true value, that value is returned; any remaining
; expressions are not evaluated. If all expressions evaluate to false, or if
; there are no expressions, then false is returned.  Install and and or as new
; special forms for the evaluator by defining appropriate syntax procedures and
; evaluation procedures eval-and and eval-or.  Alternatively, show how to
; implement and and or as derived expressions.
; ------------------------------------------------------------

(define (and-eval exp env)
  (define (and-operands-eval-recur operands)
    (let ((next (car operands))
          (rest (cdr operands)))
      (cond ((null? next) '#t)
            ((null? rest) (eval next env))
            ((eq? (eval next env) '#f) '#f)
            ((eq? (eval next env) '#t)
             (and-operands-eval-recur (cdr operands))))))
  (and-operands-eval-recur (cdr exp)))

(define (or-eval exp env)
  (define (or-operands-eval-recur operands)
    (let ((next (car operands))
          (rest (cdr operands)))
      (cond ((null? next) '#f)
            ((eq? (eval next env) '#t) '#t)
            ((eq? (eval next env) '#f)
             (or-operands-eval-recur (cdr operands))))))
  (or-operands-eval-recur (cdr exp)))

; these procedures are able to be expressed as drivatives where
(define (or-eval exp env)
  (let* ((operands (cdr exp))
         (mapped-operands (map (lambda (op) (not op)))))
    (and-eval (cons (car exp) mapped-operands))))


(define (and-eval exp env)
  (let* ((operands (cdr exp))
         (mapped-operands (map (lambda (op) (not op)))))
    (or-eval (cons (car exp) mapped-operands))))

; installing them is easy
(put 'and and-eval)
(put 'or  or-eval)
