# Multiple representations for abstract data

Up to now, we were dealing with one data abstraction. We learned how to
make data abstraction in the terms of constructors, selectors and
predicates and hide the details of physical data represenetation from
the user of the abstraction.

This way, we can have in our system many data abstractions with which
our application operates. This looks convenient but is missing one
point.

What if these data abstractions relate in the system in some way and are
not isolated entities that don't care about the other data abstractions.
What if there is some implicit knowledge about the properties of
different data abstractions in our system. How do we capture that
knowledge and make it work for us.

One of the cases when data abstractions relate to eachother is when you
have multiple data abstractions that represent same thing, but are for
any reason, expressed in a different way. One reason might be that two
departments of the company are working with different physical
representations of data, but are in fact providing same functionality.

Concrete example are complex numbers. They have two representations.
Polar and rectangular. In our example we can imagine that we have two
departments that developed two libraries for working with complex
numbers.

If we can not choose right now what we want to use, then only thing we
need is to define the set of constructors, selectors and predicates we
would like to use and then make both departments develop solutions that
use same procedures to define data abstractions. This is presented in
chapter 2.4.1

But, what if we want to use both in our system, we don't want to decide
and want to have both representations. Then, despite the fact we have
same interfaces, we need to disinguish in some way between the
representations.

A solution we use in this example is introducing the `type tags` that
tag the object with additional specification in order to distinguish the
type of the representation which is used in a case of particular complex
number instance.

In that case developers of two departments have to make their data
abstractions specific to a decided implementation and there has to be
implemented another layer of abstraction on top to provide general
terminology for all the representations and make user oblivious of the
representation details. That is done with technique called `dispatch on
type`.
