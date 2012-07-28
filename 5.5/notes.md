# Compilation

In the hardware world, there are several successful architectures that
modern computers are based on. These architectures provide set of
registers and instructions to do low level operations and control the
machine execution. In order to execute any high level program, we have
to produce the set of instructions that the machine will actually
understand. The language of the machine is called `native language`. We
say that we `compile` the high level language constructs into the native
language representation. The result of the compilation is so called
`object code`. Object program is the equivalent representation of
the program given in `source language` formed as list of `native language`
instructions.

# Interpretation vs Compilation

Both of these are strategies to make a certain machine execute
arbitrary high level code. But, they have different approaches.

Interpretation is approach where machine is raised to the level of the
high level language and understands the way language execution model is
meant to work.

Compilation on the other side is the process of analysis of the higher
level code and producing the machine code. In other words, higher level
is reduced to the needs of the machine, quite opposite of the
interpretation approach.

Compiler has one inherent advantage in performance over interpretation.
Interpreters always have to analyse code in the moment of evaluation
because they do not really know what is comming to them in the next
step. With the same regard they have to do kind of `the most pessimistic`
approach to the production of instruction set in terms of preserving the
state of the machine before evaluation of any further expression.
Compilers on the other side can analyze the code and generate the set of
instructions which are optimized for the concrete case. For eample, if
one of the arguments of the procedure is a constant, there is no need to
save whole state of the machine on the stack because evaluation of the
constant will not use any of the register. This way compilation process
can make much more compact `object code` with less isntructions to be
executed for the same code.

Compilers definitely win for the performance of execution, but
interpreters have advantage in the process of development providing much
more possibilities for the debugging. Interpreter in every moment has
the expression which is executed in the registers and stepping through
the evaluation process is easy. Compilers on the other side have to do
some effort to produce debugging information and enable debugging. LISP
systems tend to provide possibility to mix compiled and interpreted
code.


