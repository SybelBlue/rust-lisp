Plan
====


Old: `TypeContext`

New: `Solver` + `Context`

Idea
----

### Context

In a `Context`, everything has been fully typed. Referencing entries in the `Context` creates new instances of their type variables as necessary, e.g. in haskell
``` haskell
error :: String -> a

foo :: Int
foo = error "can't think of any"  -- a ~ Int

bar :: Char
bar = error "3"                   -- a ~ Char
``` 
will not throw a type error because referencing `error` during type resolution instances its generic `a` each time, i.e. `error_a_1 ~ Int` and `error_a_2 ~ Char`, so there is no type mismatch. Likewise:
``` haskell
add :: ([a] -> b) -> a -> [b]
add f x l = f (x : []) : l
--                     ^ (:) :: a1 -> [a1] -> [a1], a1 ~ add_b
--               ^ (:) :: a2 -> [a2] -> [a2], a2 ~ add_a
```

`Context`s will be the output of a module import, and the `type_mod` function.

### Solver

During type resolution, however, types are still concretizing as data is added to the `Solver`. However, the `Solver` still needs access to the previously resolved types, e.g. in the Prelude, and therefore needs an immutable reference to a `Context`.

Once the `Solver` finishes, it should add the simplest, syntactically-equivalent bindings for each of the solved types into the `Context` it references, and then release it.

`Solver`s will be private to 

Impl
----

The existing `TypeContext` will be split:
``` rust
// old version:
// pub struct TypeContext {
//     bound: HashMap<QualifiedIdent, Type>,
//     aliased: HashMap<Ident, QualifiedIdent>,
//     type_vars: HashMap<usize, Option<Type>>,
// }

pub struct Context {
    /// Types are compressed, ie all Var(_n_) inside the type start a 0
    bound: HashMap<QualifiedIdentifier, Type>, 
    aliased: HashMap<Identifier, QualifiedIdentifier>,
}

pub struct Solver {
    /// required on instantiation, released on destruction
    ctxt: Context,
    /// acyclic mapping from Var(_n_) to more concrete type in type_vars
    type_vars: HashMap<usize, Option<Type>>,
}
```