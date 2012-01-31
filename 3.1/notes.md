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

# Assignment and local state

We say that one object has state when its behavior is influenced by
history of actions taken on that object.

In order to be able to influence the history of one object and its local
state which is kept in local variables, we have to introduce assignment
operator which is a big addition to the set of features of a programming
language.
