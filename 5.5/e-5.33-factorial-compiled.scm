  (assign val (op make-compiled-procedure) (label entry18) (reg env))
  (goto (label after-lambda19))
entry18
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch23))
compiled-branch24
  (assign continue (label after-call25))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch23
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call25
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch21))
true-branch20
  (assign val (const 1))
  (goto (reg continue))
false-branch21
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
  (save proc)
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch26))
compiled-branch27
  (assign continue (label after-call28))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch26
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call28
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch29))
compiled-branch30
  (assign continue (label after-call31))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch29
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call31
  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch32))
compiled-branch33
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch32
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call34
after-if22
after-lambda19
  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
  (assign val (const ok))
