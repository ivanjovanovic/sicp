  ; make the procedure object and assign where body is and in which environment it executes
  (assign val (op make-compiled-procedure) (label entry18) (reg env))
  (goto (label after-lambda19)) ; go to after definition

; compiler will jump here when procedure is called
entry18
  ; load environment stored in procedure object
  (assign env (op compiled-procedure-env) (reg proc))
  ; extend environment with the provided arguments (which are in fact local varible assignments)
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

  ; make internal iterative procedure and assign the extended envirnment to it.
  (assign val (op make-compiled-procedure) (label entry20) (reg env))
  (goto (label after-lambda21))

; body of the internal iterative procedure, calls to it will jump to here
entry20
; here we already see a pattern in the execution.
; First build environment in which we will execute our code by extending the procedure env
; with the passed params as local variables.
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue) ; save state of the parent (one who called us and told where to go next)
  (save env) ; save our state so we know in which env we are executing

; compute (> counter n)
; Here I don't really get why we have to check if > is primitive procedure since I guess
; we can already know that in the compilation phase.
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))
compiled-branch26
  (assign continue (label after-call27))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch25
; applying primitive > to argument list we have built above.
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call27
; continue our execution after we evaluated (> counter n) by checking if result
; was true or false.
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch23))

; if true just return product from the environment as result and go wherever parent told
; us to go.
true-branch22
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))

; if false than setup for the next invocation of the procedure iter
false-branch23
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)

; compute (+ counter 1)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
; again, we should know that + is primitive
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch31))
compiled-branch32
  (assign continue (label after-call33))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch31
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

; compute (* counter product)
after-call33
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch28))
compiled-branch29
  (assign continue (label after-call30))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch28
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

; here we finish computation of the second operand of new call to iter
after-call30
  (restore argl)
; adding it to the argl
  (assign argl (op cons) (reg val) (reg argl))
; getting to see what is next to be executed.
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch34))

; here we go again to iter.
compiled-branch35
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch34
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call36
after-if24
; here we add reference to iter procedure in the environment so it is
; possible to call it after it is defined.
after-lambda21
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch37))
compiled-branch38
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch37
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call39

; here we add reference to factorial in the enclosing environment.
after-lambda19
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
