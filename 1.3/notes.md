# 1.3 Formulating Abstractions with higher-order procedures

In general, this chapter is dedicated to explanation the properties of
procedures as first-order citizens in Scheme and the way that can be
used to build up abstractions.

First-order citizen in a programming language is entity that has
following properties:

* can be stored in variables and data structures
* can be passed as a parameter to a subroutine
* can be returned as the result of a subroutine
* can be constructed at run-time
* has intrinsic identity (independent of any given name)

See [Wikipedia](http://en.wikipedia.org/wiki/First-class_object) for
more details on that.

## Lambda notation

Lambda procedures are giving a way to define anonymous functions right
in place where they are needed without the need to do their book-keeping
around application which we need for classicaly defined procedures.

### defining local variables using `let`

Based on lambda functions we can make definition of the local variables.
`let` construct is just syntactic sugar on top of lambda definition.
There are two things to be aware of let constructs

* Let allows us to bind variables as locally as possible to where they
  are used. For example:

  (+ (let ((x 3))
        (+ x (* x 10)))
     x)

  Here, if x is taken as 5 in the sum, it is still going to be 3 in the
  body of `let` construct.

* Another thing to be noticed is that variables withing the let
  procedure are calculated outside of the let body and thus are defined
  in terms of arguments outside the `let` body. For example: 

  (let ((x 3)
        (y (+ x 2)))
    (* x y))

  calculates value of `y` not based on x value of 3 but on x value of
  outside procedure.

