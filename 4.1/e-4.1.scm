; Exercise 4.1.  Notice that we cannot tell whether the metacircular
; evaluator evaluates operands from left to right or from right to left.
; Its evaluation order is inherited from the underlying Lisp: If the
; arguments to cons in list-of-values are evaluated from left to right,
; then list-of-values will evaluate operands from left to right; and if
; the arguments to cons are evaluated from right to left, then
; list-of-values will evaluate operands from right to left.

; Write a version of list-of-values that evaluates operands from left to
; right regardless of the order of evaluation in the underlying Lisp.
; Also write a version of list-of-values that evaluates operands from
; right to left.
; ------------------------------------------------------------


; list of arguments to which procedure is applied
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
                (list-of-values (rest-operands exps) env))))

; in order to make evaluation of operands from left to right we
; have to remove dependency of evaluation of the params with the
; order of evaluation in cons. So, we have to pass to cons already
; evaluated params.

; This approach for evaluating from left to right will work if we
; assume that let evaluates its variables top down. Which probably
; is as well defined by the underlying Lisp implementation?
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first-evaled-operand (eval (first-operand exps) env))
          (rest-evaled-operands (list-of-values (rest-operands exps) env)))
      (cons first-evaled-operand rest-evaled-operands))))

; still assuming that let evaluates in order, we just ahve to replace
; the order of bindings and now it will evaluate from right to left
; (from last to first)
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((rest-evaled-operands (list-of-values (rest-operands exps) env))
          (first-evaled-operand (eval (first-operand exps) env)))
      (cons first-evaled-operand rest-evaled-operands))))
