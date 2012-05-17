; Exercise 4.9.
;
; Many languages support a variety of iteration
; constructs, such as do, for, while, and until. In Scheme,
; iterative processes can be expressed in terms of ordinary
; procedure calls, so special iteration constructs provide no
; essential gain in computational power. On the other hand, such
; constructs are often convenient. Design some iteration
; constructs, give examples of their use, and show how to
; implement them as derived expressions.
; ------------------------------------------------------------

(load "../helpers.scm")
(load "4.1.scm")

; I'll take while construct as example
;
; (while <test>
;   <body>)
;
;

; example of use

; (define counter 5)
; (while (> counter 0)
;    (set! counter (- counter 1))
;    (display counter)) ; or any other body sequence
;
; It can be expanded as combination
;
; ((define iter
;     (if test
;         (begin
;           body
;           iter)))
;   iter)

; (define eval exp env
;   ...
;   (while? exp) (eval (while->combination exp) env)
;   ...)

(define (while? exp)
  (eq? (car exp) 'while))

(define (while->combination exp)
  (expand-while (cdr exp)))

(define (expand-while operands)
    (list
      (make-define 'iter '()
        (list (make-if (car operands)
                 (list 'begin
                   (cadr operands)
                   (list 'iter))
                 '())))
      (list 'iter)))


; This can be done with named let as well but that is already
; a derived construct

(output (while->combination '(while (> counter 5) (display counter))))
