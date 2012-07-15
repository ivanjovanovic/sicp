# Logic programming

Most programming languages are organized around computing the values of
mathematical functions. Thus, strongly biased towards `unidirectional`
computatis in the form of `input -> process -> output`.

Logic programming tends to revolve more around the idea of
representing `what is` part of the knowledge instead of `how to`
imperative projection of it. It is dealing more with relations between
elements combined with mighty symbolic pattern matching strategie
called `unification`.

Programms in logic programming languages are stated in the form of `what
is` knowledge, and interpreter itself provides the `how to` knoeledge in
order to derive the answer. This way we can see it as a generalized way 
for doing queries against some knowledge database.

## Logic programming concepts

Knowledge database - sets of data that we want to reason about
Query language - set of primitives for defining simple and complex
queries agains the database. This language, has as well three underlying
concepts of all the languages: primitives, means of combination, means
of abstraction.

### Simple queries (primitives)

Simple queries are used to patter match the individual records in the
database.

### Compound queries (means of combination)

Compound queries are made by combining multiple simple queries by the
means of combination rovided by the language. In logic programming
language, these are logical operators AND, OR ... like 
(and <query1> <query2> ...).
In our case there is one more mean of combination. It is lisp-value
predicate which exposes underlying lisp to the language. for example
(lisp-value <predicate> <arg1> <arg2>) would apply predicate to
arguments as we were programming in the lisp itself.

### Rules (means of abstraction)

As every progrmaming language has to have some means of abstraction,
thus logic programming can abstract compound queries as a rule in the
form (rule <conclusion> <body>). For
example:

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

Now we can use this rule `(lives-near ?person-1 ?person-2)` as a
replacement for the compound query, as we were before defining the
procedures as abstractions of several steps.

## Problems of logic programming

### Infinite loops

Because of imperfection of the implementation of the deductions, it very
easy to drive the logic programming based ruleset into an infinite loop
during the deduction process.


## Implementation of the qury system

Even though this approach to programming looks very different comparing
to what we examined before, it is based on the same `eval-apply` loop
approach. in fact, which ever programming language we take, we'll end up
having the same since it is a general approach of unwrapping
apstractions and combinations to primitives and executing them. Same is
with means of combinations and abstractions of the logic programming
language.
