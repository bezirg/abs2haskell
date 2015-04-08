# Errors in BNFC grammar

- EQualCall
- EqualVar
- No full support for annotations

# Errors in Translation

- Interface Method names clashes with non-interface (class) method names
- Right now, we use lazy IO, switch to strict IO with `evaluate`

# TODO for the translation

- Write documentation inside the source modules
- Generate haddock
- Retaining the ABS comments and generating Haskell comments
- The pattern matching is the Erlang pattern matching, which puts a requirement on Eq intances and an extra guard which can be slow
- Pointer equality using StableNames. Then we can derive
an Eq instance for each datatype and having proper equality between objects
of the same interface but of a different class. This will also enable the Erlang-style part of pattern matching for ABS.

# Comments about the ABS language for Translation

- A written method in a class that does not belong/implement any interface, cannot be called from outside.
So then what is the point of this class? => This method can only be called locally inside another same-class method.

- Method calls must explicitly be called by: object.method() or this.method(). Otherwise they point to a pure function() call.

- The type of int is unrestricted like Haskell's Integer? Then translated to Integer

# The advantages of having a Haskell backend

- Mostly (except ABS's letnonrec to Haskell's letrec) 1-to-1 translation of the functional core (datatypes + pure functions)
- 1-to-1 correspondence of OO interface inheritance and Haskell's typeclasses
- ABS and Haskell has the exact same module system (almost the same, except circular dependencies, ambiguous imports)
- Type Inference because of Milner type system
- Support for polymorphic type synonyms

# TODO enhance ABS language

- Polymorphic Methods
