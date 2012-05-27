# Normal order evaluation (lazy evaluator)

Interpreter with lazy evaluation (normal-order evaluation) is the one that
does not evaluate operands of the compound procedure during the procedure
application. Instead, the interpreter creates so called `thunks` that
enclose the possibility to `force` the evaluation of its content when
needed.
Evaluation of thunks is done upon application of the primitive
procedure, evaluation of the conditions or if thunk holds the value of
operator to be applied to other operands.

## Side effects considerations

When normal-order evaluation is used, it is important to understand how
it affects the eventual state changes if code that changes state is
passed as argument. In applicative-order evaluation the evaluation of
arguments will change state every time porcedure is called. In
normal-order evaluation, state will be changed only when actual value
is computed. And with memoizatin in place it will take in fact only
once.

## Performance considerations.

Without memoization of the thunk vlaues, every time thunk is used it
will cause evaluation of the actual value of the thunk. If this is
complicated proces it will have performance impact on overall program
execution.
