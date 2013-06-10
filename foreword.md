# Foreword

On very beginning of the lectures, authors explain clearly what is their
view on these lectures regarding naming them a part of "computer science" curricula.
It is not in fact science what is done here, and it is not either relaed
to computers only.

It is in fact about two things:

  1. How we define "what is true" - declerative knowledge
  2. How we define "how to" do something - imperative knowledge

Programming itself they see as a way to express our knowledge about
things and give an imperative explanation of how to deal with that
knowledge. What are the procedures we have to define in a programming
language that will direct a process which is going to be executed by
computer in order to do something useful.

On the other side there is a fact that software is inherently complex
and that problems that software engineering is to solve are becoming
more complex over time. Therefore, it is a task of "computer science" to
give answers on how to manage that complexity and provide some means
integrated in the programming language that can easily deal with it.

## Techniques for managing complexity

Two basic notions we deal with are:

  * Data
  * Procedures which operate on the data

All knowledge we have is represented by these, and all the techniques
are to be expresses by using data or/and procedures.

### Technique 1: Black-box abstraction

Being able to define and name new pieces of knowledge and expose them as
a black box entity in the language which can be used without knowing the
inner details is crucial thing in managing complexity. That way, we
create levels of abstractions and minimize number of items that we have
to deal with on certain level.

### Technique 2: Generalization through conventional interfaces

If we would define addition as operation in a language wouldn't be nice
if it could add two numbers as well as two electrical signals. But without
having to restructure all the code but only the operands which will
certainly be different in these cases. Defining conventional interfaces
is a way to make abstracting of operations away from the details of
operands possible.

Some approaches to this problem are:

* generic operations
* modularization
* object-oriented programming
* operations on aggregates

### Technique 3: Metalinguistic abstraction

This is in fact about creating new programming languages that are more
suitable for capturing knowledge present in some domain. It is about
creating your own tool for the purpose before using a tool meant for
something else.
