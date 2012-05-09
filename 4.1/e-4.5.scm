; Exercise 4.5.
;
; Scheme allows an additional syntax for cond clauses, (<test>
; => <recipient>). If <test> evaluates to a true value, then <recipient> is
; evaluated. Its value must be a procedure of one argument; this procedure is
; then invoked on the value of the <test>, and the result is returned as the
; value of the cond expression. For example

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

; returns 2. Modify the handling of cond so that it supports this extended
; syntax.
; ------------------------------------------------------------

; we can reuse most of the implementation of the cond as was
; defined before. Just we have to change how clauses are expanded
; to ifs
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))

; this one we change, now we have one action that should be procedure
; that accpets result of predicate
(define (cond-action clause) (caddr clause))


(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first)) ; else is a sequence of clauses
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (make-application (cond-action first) (cond-predicate first)) ; apply action to predicate result
                     (expand-clauses rest))))))

(define (make-application operator operands)
  (cons operator operands))


; this could be implemented in a way that it supports both options for defining cond clauses

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first)) ; else is a sequence of clauses
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (produce-clause-exp first)
                     (expand-clauses rest))))))

(define (produce-clause-exp clause)
  (if (eq? (cadr clause) '=>)
    (make-application (cond-action clause) (cond-predicate clause))
    (sequence->exp clause)))
