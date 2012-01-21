# Systems with generic operations

In previous chapter we have introduced the way to abstract the
differences in representation of two data abstractions that represent
the same thing. We introduced the notion of encapsulating knowledge
about relations between related data abstractions.

Here, we bring this idea to a broader level. Previously we had different
representations of complex numbers involved, now we go one abstraction
up and we make generic arithmetic which works with numbers in general.

# Coercion

After we have developed basic generic arithmetic system we see that
there are lot of flaws in the way it works. There are lot of
presumptions to be taken into account in order to use it. One of them is
that we can not sum complex with rationale numbers. We can install all
combination of procedures into all the packages to handle these cases
but there is a better way to do it. It is called coercion.

# Hierarchies of types

Coercion is a way to solve conversions between types that have natural
relations. Often there are more complex ways types relate.
