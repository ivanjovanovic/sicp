# Register machines

Two basic concepts in the topic of register machines are `data paths`
that are registers and operations and `controller` which represents
sequence in which these operations are executed.

## Language for describing register machines

In order to effectively describe the register machine, we will introduce a 
language specialized for this purpose. 

Controller of the register machine is defined as a sequence of
`instructions` together with set of `labels` that identify some
significant entry points in the sequence.

Instruction can be one of these:

* assignment of value to the register (`assign` instruction)
* test instruction for testing the state of certain register (`test`
  instruction)
* conditional branching (`branch` instruction) of controller execution path based on the outcome
  of the given test instruction.
* Unconditional branch (`goto` instruction)

## Stack

When we introduced procedural abstraction in some of the first chapters, we introduced the notion of procedure which contains a list of steps to be executed when it was called. In that moment we did'nt care about how program knows how to come back to the position where it called the procedure and how to return the output of the computation to the calling procedure.

Now, seeing how register machine works, we see a need for some place
where `state` of the controller will be saved and restored from when we
enter exection of the subroutine. This place where state is saved we
will call `stack`. Stack is "last-in first-out" data structure. And data
gets on the stack by being `saved` and taken out when `restored`.
