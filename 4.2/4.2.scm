(load "../4.1/easy-scheme.scm")



; the main change in the evaluation is in the application process
; where we don't evaluate operands
;((application? exp)
 ;(apply (actual-value (operator exp) env)
        ;(operands exp)
        ;env))


(define (eval exp env)
  (debug-log "EVAL-ENTER" exp)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))

        ; the main change in the evaluation is in the application process
        ; where we don't evaluate operands
        ((application? exp)
         (begin
           (debug-log "APPLICATION" exp)
           (apply (actual-value (operator exp) env)
                  (operands exp)
                  env)))
        (else
          (error "Unknown expression type -- EVAL" exp))))

; where for the resolution of operator we have to force a thunk

(define (actual-value exp env)
  (begin
    (debug-log "ACTUAL-VALUE" exp)
    (force-it (eval exp env))))

; new version of apply which differentiate how compund
; and primitive procedure application differs is to be implemented

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; where, now we have different way to get the argument values for
; primitive and compound procedures.

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
          (list-of-arg-values (rest-operands exps)
                              env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env)
          (list-of-delayed-args (rest-operands exps)
                                env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))



; thunks are objects that keep the expression to be evaluated
; when needed. They are created by
(define (delay-it exp env)
  (begin
    (debug-log "THUNK-CREATION" exp)
    (list 'thunk exp env)))

; and evaluation is forced by
(define (force-it obj)
  (begin
    (debug-log "THUNK-FORCING" obj)
    (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj)))

(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

; we can be a bit smarter than this and memoize the
; value of the thunk after first evaluation and then
; just use computed value

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define the-global-environment (setup-environment))
(driver-loop)
