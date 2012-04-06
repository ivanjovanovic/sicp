# Concurrency: Time is of the essence

By introducing state, we introduced to notion of change, which is
defined as difference between the state of something before and after
certain point in time. Therefore, we introduced the notion of `time`.
According to this, we can not reason about our programs anymore only by
the code that is in certain portion of the programm, but we need to know
in which point of execution time are we reasoning about it because in
different moments, same code can produce different results. We
introduced extraordinary complexity.

Somehow this is manageable, until we start understanding that in real
world, things don't happen sequentially but in parallel to each other,
and that we have to model somehow this parallelism in our programs. In
this case, notion of time becomes even more complex.

Core of subtle problems in concurrent programs is mutation of the state
of variables that are shared among multiple concurrent processes. We
already remember that when using `set!` we have to care about the order
of mutations, the same applies here.

In order to prevent this kind of incorrect behavior we have to impose
certain restrictions in the way we reason about and implement concurrent
programs.

* One way is to forbid by the design that any of the mutators can
  change in same time any state that is shared among processes. This is
rather inefficient.
* Less stringent restriction is to ensure that mutations occur as they
  were executed sequentially. In this case we might have multiple
answers at the end, but programm will still be correct.

## Mechanisms for controlling concurrency

One practical approach of concurrency control is serialization of the
access to the shared state. It implements the following idea: Processes
can execute concurrently, but certain procedures can not execute in the
same time. If one serialized procedure is in execution and other tries
to start, it will have to wait until the first has finished.

## Implementation of serializers

Serializers are implemented as computational objects that contain more
primitive synchronization mechanism called a `mutex`. Mutex is an
computational object that has two operations, it can be `aquired` and
`released`. Once aquired, other that are to be synchronized have to wait
until is released to aquire it.

## Deadlock

Even when we have serialization implemented, it is possible that two
processes have a special case in which first depends on the resource
locked by second, and second depends on the resource locked by the
first. In this case they are indefinitely waiting for each other. We say
they are in the deadlock.
