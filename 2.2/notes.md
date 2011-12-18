# Hierarchical Data and Closure property

Main point of data abstraction is to separate
* the way how something is used from
* the way something is defined

In LISP we could have seen this on example of `cons` which is used as
container for pairs of arbitrary data, but is in fact defined as
procedure, and it could be defined in different ways which would not
change the way something is used.

LISP provides `pairs` as a mean of data abstraction. This data
abstraction provides `closure property` which means that you can make
more complex data by combining already combined data. For example, we
can make pair of pairs by using the same mean of combination (making `pairs`).
This way, we can build very complex data abstractions with simple mean
of combination provided by the language.

This is one of the essential questions about the means of combination in
some programming language. We should always ask

> Is this mean of combination closed over the set of primitive elements
of the langauge.

For example if you can not create array of arrays as in FORTRAN, then we
can not say that this language has `closure property` of this mean of
combination.

## Sequences

First, and most simple way to combine data is in the sequence. As the
ordered collection of data objects. Such ordered collection is called a
`list`.

One technique to walk through the list is `cdr-ing down` the list.
Other technique is to `cons up` the resulting list while `cdr-ing down`
given input list.

### Mapping over lists

One of basic operations is to apply certain transformation to all
elements of the list.

Introducing `map` abstraction is very good example of how adding layers
of abstraction moves our thining into different directions. If we take
example of scaling list of items as 

  (define (scale-elements items factor)
    (if (null? items)
      nil
      (cons (* (car items) factor) (scale-elements (cdr items) factor))))

and with map abstraction with

  (define (scale-elements items factor)
    (map (lambda (x) (* x factor))
         items))

we can see that in second case we don't have to think about the details
of impemenatation of scale-elements procedure since map is well
understood abstaraction which stands in front of all the details of
implementation.

## Hierarchical structures

As we can represent sequences of numbers we can similarly represent
sequences of sequences. For example (cons (list 1 2) (list 3 4))
represents pair of lists. Alhtough printed as ((1 2) 3 4), if we
represent it a bit differently we can see its hierarchical nature

             ((1 2) 3 4)
                /    |\
               /     | \
        (1 2) /|     |  \
             / |     |   \
            1  2     3    4

This looks like representation of the kind of tree. In fact,
hierarchical data representation is used to represent so called `tree`
data structures most often.

Trees have `length` and `number of leaves` properties among the others.
Recursion is natural way of dealing with these data structures.

### Mapping over trees

As trees are one of basic data structures, mapping over them is one of
the most basic operations.

On example of factoring all the leaves of the tree we can show how
mapping on trees works

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree 
  (list 1 (list 2 (list 3 4) 5) (list 6 7)) 
  10) -> (10 (20 (30 40) 50) (60 70))

Another way to implement scale-tree is to regard the tree as a sequence of
sub-trees and use map. We map over the sequence, scaling each sub-tree in turn,
and return the list of results. In the base case, where the tree is a leaf, we
simply multiply by the factor:

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

Many tree operations can be implemented by similar combinations of sequence operations and recursion.

## Sequences as conventional interfaces

Lot of examples in designing software comprise of sequence of operation
that have to be executed one after each other in order to fullfil the
goal. Or at least, software can be organized that way. Easiest is to
compare with engineering approach to designing signal processing system:

  --------     --------     --------    --------
  |enumer.|    |filter.|    |  map  |   |accumu.|
  |       | -- | odd?  | -- |square |-- | +, *  |
  |       |    |       |    |       |   |       |
  --------     --------     --------    --------

in these systems signal is a common carrier of the data, and it flows
through the processing blocks giving an desired output after the last
block.

Complex software operations can be broken into blocks that operate on
this signal. This way we get general reusable blocks of software, and a
conventional interface between these blocks.

In Scheme lists are used as data carriers and conventional interfaces.
Block functions can be many, mapping, filtering, accumulating ...

Conslusion is that we can encourage flexible and modular designs by
bulding intepented blocks that communicate over conventional
interfaces.
