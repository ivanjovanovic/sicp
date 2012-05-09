; Exercise 4.6.
;
; Let expressions are derived expressions, because

; (let ((<var1> <exp1>) ... (<varn> <expn>))
;   <body>)

; is equivalent to

; ((lambda (<var1> ... <varn>)
;    <body>)
;  <exp1>

;  <expn>)

; Implement a syntactic transformation let->combination that reduces evaluating
; let expressions to evaluating combinations of the type shown above, and add
; the appropriate clause to eval to handle let expressions.
; ------------------------------------------------------------
(load "../helpers.scm")
(load "4.1.scm")

(define (let->combination exp)
  (expand-let (let-operands exp)))

(define (let-operands exp)
  (cdr exp))

(define (expand-let operands)
  (let ((parameters (let-parameters (car operands)))
        (parameter-values (let-parameter-values (car operands)))
        (body (cdr operands)))
    (make-application
      (make-lambda parameters body)
      parameter-values)))

(define (let-parameters bindings)
  (if (null? bindings)
    '()
    (cons (caar bindings) (let-parameters (cdr bindings)))))

(define (let-parameter-values bindings)
  (if (null? bindings)
    '()
    (cons (cadar bindings) (let-parameter-values (cdr bindings)))))

(define (make-application operator operands)
  (cons operator operands))

; (output (let-parameters (list (cons 1 2) (cons 2 3) (cons 3 4)))) ; (1 2 3)
; (output (let-parameter-values (list (list 1 2) (list 2 3) (list 3 4)))) ; (2 3 4)

; (output (let->combination '(let ((a (+ 5 1)) (b 5) (c 5)) (cons 1 2)))) ; ((lambda (a b c) (cons 1 2)) (+ 5 1) 5 5)

(define (eval-let exp env)
  (eval (let->combination exp) env))

(define (let? exp)
  (eq? (car exp) 'let))

; (define (eval exp env)
;   ...
;   ((let? exp) (eval-let exp env))
;   ...
;   )
