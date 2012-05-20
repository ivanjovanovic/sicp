; Exercise 4.16.
;
; In this exercise we implement the method just described for
; interpreting internal definitions. We assume that the evaluator supports let
; (see exercise 4.6).

; a.  Change lookup-variable-value (section 4.1.3) to signal an error if the
; value it finds is the symbol *unassigned*.

; b.  Write a procedure scan-out-defines that takes a procedure body and
; returns an equivalent one that has no internal definitions, by making the
; transformation described above.

; c.  Install scan-out-defines in the interpreter, either in make-procedure or
; in procedure-body (see section 4.1.3). Which place is better? Why?
; ------------------------------------------------------------

(load "../helpers.scm")
(load "4.1.scm")
(load "e-4.7.scm")

(define (make-let bindings body)
  (list 'let bindings body))

; a
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
               (error "Unasssigned variable: " (car vals))
               (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))
  (env-loop env))

; b

(define (scan-out-defines body)
  (append (list 'let (unassigned-bindings body))
            (map convert-defs-to-assignments body)))

(define (unassigned-bindings body)
  (cond ((null? body) '())
        ((eq? (caar body) 'define)
         (cons
           (list (cadar body) '*unassigned)
           (unassigned-bindings (cdr body))))
        (else (unassigned-bindings (cdr body)))))

(define (convert-defs-to-assignments body-sequence-element)
  (if (eq? (car body-sequence-element) 'define)
    (list 'set! (cadr body-sequence-element) (caddr body-sequence-element))
    body-sequence-element))

(define input '(lambda ()
                 (define t (+ 3 2))
                 (define p (car (cons 1 2)))
                 (display "test")))

;(output (scan-out-defines (cddr input)))

; c
;
; I will install in in the make-procedure since procedure body is used more often to take out the body
; of the procedure and I don't want this executed every time.

(define (make-procedure body env)
  (list 'procedure parameters (scan-out-defines body) env))
