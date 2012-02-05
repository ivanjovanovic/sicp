# Modeling with mutable data

Until now, we have defined data abstraction as data hidden behind
procedures that acted as constructors, selectors and predicates.
Now, we introduce possibility to change the data, therefore in our
abstraction we introduce another kind of procedure, mutator.

Data objects whose data can be mutated are called `mutable data objects`

## Sameness and change

When we introduced assignment we raised rather philosophical questions
of sameness and change of the object. Now, we get from phylosophy to
real examples. Since we can wire the data with mutators, we have to take
care of what our objects are in certain moment and to what are they
same, and what happens if they change. This is especially with `shared`
objects, that re used by two or more different complex structures.

A way to detect if two objects are same must be included in the
programming language. In Scheme procedure (eq? x y) will tell us
if x and y point to the same object.

