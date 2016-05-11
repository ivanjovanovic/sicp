# 1.1 The Elements of programming

If you are asked what are three basic things that make computer language
powerful you should find the answer in three questions:

* What are the the most primitive expressions that come with the
  language?
* What are means of combination of primitive expressions provided by the
  language?
* What are means of abstraction or how to create new primitive elements
  that are to be used in building more complex structures?

Lisp is specific as it provides only a small set of primitives and very
powerful ways of combining them and creating new abstractions. `3` is a
primitive as well as operator `+`. Combination

    (+ 3 4)

is a way to combine primitives and express a wish to get sum of these
two primitives. Here `()` are the mean of combination of operation and
operands.

To make new abstractions we need a way to define new primitives in a
language. In Lisp we do this by using i.e. `(define a (+ 2 3))`. Here we
define new symbol `a` which is represented as a sum of 2 and 3 and is in
fact value of 5.

define primitive enables us to define procedures as well in two ways

    (define (square x) (* x x))

where implicitly we do this in fact

    (define square (lambda (x) (* x x)))

Here we defined a `square` primitive which captures the knowledge of
getting square of a number.

In Lisp there is only one more primitive which enables us to make
decisions within our procedures. On example of defining absolute number

    (define (abs x)
      (cond ((< x 0) (- x))
            ((= x 0) 0)
            ((> x 0) x)))

Or differently it can be written as an `if` primitive which is
equivalent to previous definition.

    (define (abs x)
      (if (< x 0) (- x)
          x))

## Procedure substitution model

To evaluate statements whose part are operators that form compound
procedures to primitive ones we use so called substitution models. There
are two of them.

### Applicative order evaluation

This substitution model is based on the process of substitution which is
based on next steps:

* Evaluate the body of the compound procedure
* Evaluate the corresponding arguments for the new procedure

Example:

    (f 5)

evaluates compound procedure `f` to

    (sum-of-squares (+ a 1) (* a 2))

after we replace corresponding arguments

    (sum-of-squares (+ 5 1) (* 5 2))

now that becomes

    (+ (square 6) (square 10))

and

    (+ (* 6 6) (* 10 10))

then

    (+ 36 100)

which finally gives `136` as the result.

All together:

    (f 5)
    (sum-of-squares (+ a 1) (* a 2))
    (sum-of-squares (+ 5 1) (* 5 2))
    (+ (square 6) (square 10))
    (+ (* 6 6) (* 10 10))
    (+ 36 100)
    136

### Normal order evaluation

Normal order is different in the sense that it doesn't evaluate
corresponding operands until substitution process evaluates all the
procedure bodies (operators) to the primitive operations. Example

    (f 5)

evaluates to:

    (sum-of-squares (+ 5 1) (* 5 2))

further expands to

    (+ (square (+ 5 1)) (square (* 5 2)))

becomes

    (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))

which is now reduced to following

    (+ (* 6 6) (* 10 10))

then

    (+ 36 100)

which is again `136` as the result

All together:

    (f 5)
    (sum-of-squares (+ 5 1) (* 5 2))
    (+ (square (+ 5 1)) (square (* 5 2)))
    (+ (* (+ 5 1) (+ 5 1)) (* (* 5 2) (* 5 2)))
    (+ (* 6 6) (* 10 10))
    (+ 36 100)
    136
