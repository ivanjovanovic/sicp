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

## sets as ordered lists

One way to improve performance of the operations on a set is to
represent set with ordered list. For the simplification we will work
with numbers, but we could have any object which could be comparable
with other of same kind. This way, we will not need to traverse whole
set every time but only until we are sure that it is useful.

Checking if something is element of set still needs in worst case to
traverse whole list to give the answer but in averae it is O(n/2) which
is stil linear with number of elements but with less steep slope. it is
factor of 2 in average less.

Intersection gives even better results. Given the ordered sets we can
then say that when x1 is equal to x2 reset to compare is only (cdr set1)
with (cdr set2) which is less than always comparing element of set1 with
whole set2. When x1 is smaller than x2 then we can immediatelly tell it
is not part of the intersection. All of this makes intersection growing
with the rate of O(n) rather than O(n^2) in the case of unordered sets.

## Binary trees

Ordered lists are good, but we can do better than that if we organize
our list in the form of the tree. Every node of the tree will hold one
element of the set and two (possibly empty) links. `left` link will hold
link to a node that contains smaller element and `right` link will hold
the link to a bigger value. There are various ways to represent the set
in the tree form defined above. So we can have different trees for the
same set.

If tree is set this way, than to search for the element of the set, we
can eliminate whole subtree in evry step since we know that on the left
we have lower values and on the right bigger ones. If tree is balanced
then searching this tree is done with the O(log n) steps, given the size
of the set n.

Having simple procedures that adjoin elements to a set do not guarantee
having balanced tree as result. Without the balance in the tree we are
using the property of efficiency which make possibility to search and
add elements in the tree in the O(log n) number of steps.
