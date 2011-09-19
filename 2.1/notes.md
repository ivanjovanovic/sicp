# Introduction to Data Abstraction

As already said, every programming language provides primitives, and some
means of their combinations. With procedures it was (define) primitive
that was enabling us to name new concept in our program in terms of groups
of primitive operations and then treat all this group as one entity by its given name.

The same idea is about `data abstraction`. For example, if we take
rational numbers. One such is made out from 2 integers named `nominator`
and `denominator` represented as n/d. If we would write the summation
operator on these two numbers like this.

r1 + r2 = n1/d1 + n2/d2 = (n1*d2 + n2/d1)/d1*d2

If we look it from left to right we can see that on the very left we are
looking at familiar sum with two rational numbers r1 and r2.
If we look at the very right, we see expanded version of this summation
in terms of nums and dens. What is visible is that here we see a lot of
details of internal representation of the rational numbers structure and
we can not even see from the first look that it is summation.

For the left side, we say that we use `abstract representation` of
rational numbers, without even knowing anything about the details of
implementation of them. And that is all what `data abstraction` concept
is. It is about using data in an abstract way that separates what data
is, from how data is implemented.

One way to detect working with wrong level of data abstraction is if in
your usage of data you have to deal with details of data structure,
usually that ends up with creating lot of temporary variables and glue
code to handle data structure details. On the other side, dealing with
proper levels of data abstraction is about focusing on the actual
problem we want to solve with the data without worring about the
internals of data.

When building data abstractions it is useful to use strategy of `wishful`
thinking. When we implement code that uses our data abstractions, we
should work with them as we already have them, without using their
details. In that way we will implement usage of it in an abstract way,
and the way they are really implemented afterwards will not be that
important anymore.

## Pairs

In Scheme, basic way of combining or `gluing` data together is with
creating pairs with constructor (cons 1 2), which will construct list of
numbers (1 2)

## What is data?

In general, we can think of data as defined by some collection
of selectors and constructors, together with specified conditions
that these procedures must fulfill in order to be a valid
representation. 

One thing to notice about the definition of the data is that we do not
explicitely tell anything on content, numbers, strings or some phisical
data representation. 

  `data abstraction = constructor + selectors + contract`

And therefore, data can be anything. In example of pairs it is set of 
procedures that behave in a certain way to implement abstraction around
concept of pairs.

## On Church numerals

In exercise 2.6 Church numerals are introduced. In everyday software
engineering and computer science, they are not of some practical
use. They are used in theoretical CS more often. From the other side
they explicitely expose the fact that even usual primitive data
representations as numbers could be represented as higher order
procedures if language supports them. Usually this is not going to be
done due to many practical reasons, but just a fact that number can have
such a different representation is fascinating and mind opening for 
looking at data in completely different way.

[Church Numerals](http://en.wikipedia.org/wiki/Church_encoding#Church_numerals)
[Nice blog post and discussion](http://briancarper.net/blog/479/)
