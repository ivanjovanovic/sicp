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

; installing them is easy
(put 'and and-eval)
(put 'or  or-eval)

; they can be expressed as well as the derived from if expressions

; and related procedures
(define (and-eval exp env)
  (eval (and->if exp) env))
(define (and->if exp)
  (expand-and-operands (cdr exp)))
(define (expand-and-operands operands)
  (let ((first (car operands))
        (rest (cdr operands)))
    (if (null? rest)
      (make-if first
               first
               'false)
      (make-if first
               (expand-and-operands rest)
               'false)))

; or related procedures
(define (or-eval exp env)
  (eval (or->if exp) env))
(define (or->if exp)
  (expand-or-operands (cdr exp)))
(define (expand-or-operands operands)
  (let ((first (car operands))
        (rest (cdr operands)))
    (if (null? first)
      'false
      (make-if first
               'true
               (expand-or-operands rest)))))

; these procedures are able to be expressed as derivatives of themselves
(define (or-eval exp env)
  (let* ((operands (cdr exp))
         (mapped-operands (map (lambda (op) (not (eval op env))))))
    (and-eval (cons (car exp) mapped-operands) env)))


(define (and-eval exp env)
  (let* ((operands (cdr exp))
         (mapped-operands (map (lambda (op) (not (eval op env))))))
    (or-eval (cons (car exp) mapped-operands) env)))

