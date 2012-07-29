; Exercise 5.32.
; Using the preserving mechanism, the compiler will
; avoid saving and restoring env around the evaluation of the operator
; of a combination in the case where the operator is a symbol. We could
; also build such optimizations into the evaluator. Indeed, the
; explicit-control evaluator of section 5.4 already performs a similar
; optimization, by treating combinations with no operands as a special
; case.

; a. Extend the explicit-control evaluator to recognize as a separate
; class of expressions combinations whose operator is a symbol, and to
; take advantage of this fact in evaluating such expressions.

; b. Alyssa P. Hacker suggests that by extending the evaluator to
; recognize more and more special cases we could incorporate all the
; compiler's optimizations, and that this would eliminate the advantage
; of compilation altogether. What do you think of this idea?
; ------------------------------------------------------------

; optimized application that takes symbol case into account.
ev-application
  (save continue)
  (assign exp (op operator) (reg exp))
  (test (op symbol?) (reg exp)) ; testing if it is a symbol
  (branch (label ev-apply-symbol-operator)) ; going directly to the building arg list
  (save env) ; oly now we should save to stack when we know there is something to be evaluated
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-evaluated-operator
  (restore unev)
  (restore env)
ev-apply-symbol-operator
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)

; no changes below
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

; b) Compilation would be even then better since it will not need all
; the checks which are implemented. As well interpreter doesn't really
; know what is going to happen next and it has to check for the type of
; the expression every time before even comming to execution of the
; code. With compilation this is not needed because that is done only in
; the compilation phase and then what is left is just pure execution of
; the instructions.
