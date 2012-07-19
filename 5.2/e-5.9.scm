; Exercise 5.9.
; The treatment of machine operations above permits them to
; operate on labels as well as on constants and the contents of registers.
; Modify the expression-processing procedures to enforce the condition that
; operations can be used only with registers and constants.
; ------------------------------------------------------------

; we have to modify the place where expressions for the operands of
; the operations are converted to execution procedures and prevent
; operations on labels.

; operation expression
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (not (or (register-exp? e) (constant-exp? e)))
                  (error "This expression is not allowed as operand -- MAKE-OPERATION-EXP" e))
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
