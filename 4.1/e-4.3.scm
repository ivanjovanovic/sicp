; Exercise 4.3.
;
; Rewrite eval so that the dispatch is done in
; data-directed style. Compare this with the data-directed
; differentiation procedure of exercise 2.73. (You may use the car of a
; compound expression as the type of the expression, as is appropriate
; for the syntax implemented in this section.)
; ------------------------------------------------------------

; If we look at the structure of the evaluator, we see that it has a pattern
;
; 1) we evaluate the condition to see which procedure to use as evaluator.
; 2) we apply evaluator procedure to the expression in certain environment
;

; similar to 2.5.scm we define the table to hold the mappings
(define eval-table (make-hash-table))
(define (put type val) (hash-table-set! eval-table type val))
(define (get type) (hash-table-ref/default eval-table type #f))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (looup-variable-value exp env))

        ; if procedure is registered for the type of expression apply it
        ((not (null? (get (car exp))))
         ((get (car exp)) exp env)) ; get the procedure object and apply it

        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown type of expression -- EVAL" exp))))


; now we just have to fill the table with the procedures we have already defined
; for the specific expression types.

(put 'quoted
     (lambda (exp env) ; have to make adapter for the generic procedure signature
       (text-of-quotation exp)))
(put 'set!  eval-assignment)
(put 'define eval-definition)
(put 'if eval-if)
(put 'lambda (lambda (exp env)
               (make-procedure (lambda-parameters exp)
                               (lambda-body exp)
                               env)))
(put 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'cond (lambda (exp env) (eval (cond->if exp) env)))
