# Resource allocation and garbage collection

Overall it is important to understan that higher level languages operate
on the abstractions that have their own physical representations. Every
time we do (cons x y) it build a new pair that is placed for us
somewhere in the memory.
There isa way objects as lists, procedures and rest are represented in
the physical way, so we can say they are abstractions on top of their
physical representation.

## Garbage collection

Since we use some physical space to store our objects, it is very
important to use that space economicaly since it is not free and
unlimited. Differet strategies are developed to reuse the memory that is
not needed anymore by the certain program, and this concept is called
garbage collection.
