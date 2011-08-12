# 1.2 Procedures and the Processes They Generate

Important thing here is to note separation between two concepts.

1. Procedure which define processes
2. The nature of the process itself.

For example, it can be seen that procedure might represent recursive
definition of the process, but process itself doesn't have to be
recursive. It might be iterative in its nature.

Two types of processes that we can differentiate are 

* Iterative
* Recursive

If orders of growth are defined in linear polynomial notation like O(n)
we say call processes linear.

Some langauge implementation can be `smart` and figure out that process
is iterative and even if it is defined by recursive procedure optimize
and execute as linear iterative process. This is called [tail recursion](http://c2.com/cgi/wiki?TailRecursion) 
Usually process can be executed as iterative if is defined recursevly
but after finishing the recursion it doesn't do anything but just
returning the value.

## Linear recursive

time = O(n)
space = O(n)

This kind of process has pars of the state in variables that are out of
local scope of execution and are in fact part of the process.

## Linear iterative

time = O(n)
space = O(c) - constant

This kind of process has all the states that define it captured in local
variables and could be restarted with provided local variables in any
time.


## Tree recursion

Common pattern of recursive computation.
