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

# Constraint based programming

Generally, computer programs are perceived to be implementd to take some
input, process it and return the result on the output. There are, from
the other side, systems that are modeled in terms of relations within
quantities and where change of one quantity by some kind of law
(constraint) stays valid only if other quantities are reshaped to adapt
to the change of this value.
Laws of physics are maybe the most obvious example, like F = m*a where
law defines relation between values.
Such relations are not unidirectional. Change of any quantity can be
input in such a case.
Modeling such a system in computer requires a way (language) to express
these relations. Relations are expressed through `constrainst` which are
connected in a `constraint network` via `connectors`.
See more: http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html#%_fig_3.28

This concept is very natural to how some systems work and there is no
much friction between the representation in computer and how system in
reality really works.
Optimization problems, like Knapsack problems can be solved using
constraint based approach for optimization.

Good starting point for exploration can be:
http://en.wikipedia.org/wiki/Constraint_programming

Good source for problem ideas come from the Operations Research domain:
http://en.wikipedia.org/wiki/Operations_research
