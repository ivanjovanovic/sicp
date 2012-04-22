# Streams

With the assignment, we have introduced:
* state
* change of state (time)
* identity of elements
* impotnace of ordering of events
* sharing of state between elements
* The most important thing, we have lost the possibility to use
  substitution model for evaluation.

On the other side we saw that state is of great significance for
modeling certain abstractions in way that allows us to isolate their
effect on the system. Somehow obects with the internal state are the
closest to how we see the nature, so it is very close to native way of
thinking about how to represent nature in the software.

## Signal processing view of the world

On the other side, there is a different way to look at the world. More
like a set of boxes that process the signal and for given input
produce certain output. This signal is called a `stream`. It is a data
abstraction that is abstracted by `cons-stream`, `head-stream` and `tail-stream`.
They are almost like lists, just different in an essential way.

In this view of the world, streams represent a conventional interface
for communication between elements of the program, and in that way as
lists do they unify the communication between differents abstractions.

## Benefits of streams

Stream based approach allows us to use standard approach to processing
of elements based on

    enumerate -> map -> filter -> accumulate

approach. For example, in the iterative approach of finding solution to
the 8 queens problem, we would go the deepest down one branch of the
tree of the solutions and then go back up again and then go down deep
again until we iterate through all the solutions. In the stream based
solution we could enumerate all possible options for next field, then
filter only the possible solutions, and then in fact solve them going
level by level in the tree of solutions without going deep and
backtracking. this way solution is reduced to the standardized form
which is easily explainable and understandable and properly abstracted
in decoupled procedural abstractions.

## Cost of streams

For example, if we would like to generate signal that represents stream
of integers of value from 1 to 1.000.000 and to process it through
several program elements, we would need to generate million of them, and
then pass them through every step. This is, before all, memory consuming
and CPU intensive, and often there is no need to do that.

## Solving the costs of streams

The previously explained problem is solvable by using the recursive
definition of the stream, where stream consists of value in the head,
and the tail that is not anymore list of values but only the procedure
that promises to calculate next value when called. Defined by the
abstraction.

With this approach we have hidden the state of the stream behind a
function that is now always the same, and has stable interface which
promises to return something when called. Now we can again use the
substitution model of evaluation.

## Key idea

Key idea about this is that by using stream and delayed evaluation of
the stream tail, we have decoupled the order of events in computer from
order in our program. So, events in our program happen when we want and
not the way it is predefined.

## Cost of this solution

By introduction of delayed evaluation of the stream tail, we have
introduced as well one nasty side effect. We introduced two types of
procedures, oe normal and other delayed and when building higher-order
functions we have to know if we are passing one or another. That is kind
of specialization of procedures that will bring us many problems.
One solution to this is to make them all delayed. In order to use this,
we have to change the model of evaluation to `normal-order evaluation`
which has again some nasty side effects in the form that that way we can
not anymore use iteration ...


## Stream based modeling of the world vs mutable state based.

Presented way of looking at the world is a counter part of the approach
to modeling world based on objects with internal mutable state. Both of
them introduce sideffects that ruin dramatically pure mathematical
properties of programs and lead to models that are harder to understand,
reason about and maintain. So there is no ground to say that any of them
is perfect or the right approach for every problem.

## Infinite streams

During solving the exercises from the section related to infinite
streams, we have developed small library for arithmetic with streams and
wth Taylor series for representation of the math functions. Here is
interesting to see how naturally streams fit as underlying data
structure some concepts and how easy is to build on top of them to
produce more complex abstractions.

## Exploiting stream paradigm

Beside the fact that streams are great as underlying data abstraction
for concepts which are naturally represented as infinite series of data,
streams can be used for several more purposes.

### Iterations as stream processes

As we know, iteration is a process that is described as set of actions
that are conducted iterativelly on set of variables (set meaning one as
well) which change their state in every iteration. Therefore usually we
have some kind of assignement of new state to the variables on beginning
or end of iteration ...Other approach is to hide the change of the state
of variable into the stream and in every iteration just take next
element from the stream. This way we separate set of operations that are
to be conducted and the inputs to this set of operations for every
iteration.
