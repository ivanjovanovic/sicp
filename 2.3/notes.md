# Symbolic data

Until now we were using expressions and numerical data as part of our
programs. Beside them we can use as well symbols, which are data that
literally stands for itself. Having no value and not representing the
expression which is evaluated, just existing in the system the way it
is.

## Quotation

Symbols are created by quoting the expressions. Like '(a b c).
In Scheme standard primitive operations on lists can be used as well on
the symbolic data itself.

# Representing sets

One of the abstractions that can represent a set is defined by following
set of procedures: `union-set`, `intersection-set`, `element-of-set?`, and `adjoin-set`.
As we learned before, behind these abstractions we can implement
different representations for the sets of data.

One thing, important to understand here, is the concept of efficiency of
the representation of the data behind the abstraction. In this example
we can see how different representations of sets carry with them
different properties in terms of efficiency and complexity of
implementation. As by the rule, the more simple implementation the less
efficient it is.

## sets as unordered lists

In this case, set is represented as list of items in which element can
not appear more than once. Implementation of given abstractions for this
representation are relying on checking every time if element of the set
is in the set, or in the other set in case of set arithmetics.
Therefore, adding element to the set will be of O(n) of complexity and
union or intersection will need O(n^2) steps for the set sizes of n.


