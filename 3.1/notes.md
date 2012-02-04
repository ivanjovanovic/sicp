# Modularity, Objects and State

Previous material introduced us to building abstractions by combining
data and procedures. We got introduced to a kind of mathematical way
of thinking about the computation. With this chapter we will introduce
feature of the langauge that breaks apart the beautiful computational
model we worked with.

Introducing new feature in a language has to have good reason,
especially if it breaks elegance of the computation model as our case
is. Good reason for introducing a feature is to enable user of the
language (programmer) to build abstractions in a novel way which
otherwise would not be possible. Introduction of the state enables us to
enable bigger modularity of the application and to introduce better
separation of concepts and make more coherent abstractions in the place
where state is necessery and intrinsic to the defined abstraction.


With introducing state in the model we are doing exactly that, but we
have to be aware that we should not forget the power and all good that
functional approach to programming brings but we should see introduction
of changing state only as tool for the cases where it is needed.

As said, we will hae to introduce new model of computation, which will
be defined and esplained as environmental model of computation.

## Assignment and local state

We say that one object has state when its behavior is influenced by
history of actions taken on that object.

In order to be able to influence the history of one object and its local
state which is kept in local variables, we have to introduce assignment
operator which is a big addition to the set of features of a programming
language.

By being able to have local state and assignment operator we can produce
simple objects like this

  (define (make-withdraw balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds")))

or more complex ones that have more than one action associated with
internal state, like this.

  (define (make-account balance)
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch)

this can be used like this
  (define acc (make-account 100))

This way of resolving the action to be taken on an object is called
message passing style.
  ((acc 'withdraw) 50))

## Benefits of introducing assignment

Modeling a system with objects that contain local state enables us to
make the system design more modular and broken into more maintainable
pieces whose changes do not affect other parts of the system.

## Costs of introducing assignment

* Loss of simple substitution models for evaluation of our programs.
* Since we introduce change, we introduce the question of what is the
  meaning of "same" in our language. When is something "same" and to
what is it same
* We can not anymore easily substitute `equals` for `equals` since we
  don't exactly know in a point of time what is equal. This means we are
losing `referential transparency` of our language.
* Informal reasoning of the programs get difficult
* Formal reasoning of the language gets difficult
