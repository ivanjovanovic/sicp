; Exercise 5.23.
; Extend the evaluator to handle derived expressions
; such as cond, let, and so on (section 4.1.2). You may ``cheat'' and
; assume that the syntax transformers such as cond->if are available as
; machine operations.28
; ------------------------------------------------------------

; we have to install the basic operations in the machine during the
; machine definition. These will be taken from the definition of the
; metacircular evaluator.

(list (list 'cond? cond?)
      (list 'cond->if cond-if))
; to install it in the evaluator dispatcher
eval-dispatch
  ...
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  ...

; to replace current expression with its substitue based on
; if and to jump to evaluation of if expression
ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label ev-if))
