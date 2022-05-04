rust-lisp
==============

This was made to help me practice PLD/I concepts as I learn them.


Language Goals
--------------

- Complete Static Type Inference
- Sound Type System
- Type Erasure
- Compiled (!?)

Implies
- _No Dependent Types_

Current Syntax Spec
------------------

``` lisp
;; lambda (expr expr)
(x -> x)
;; bind
(x <- 3)
((double x) <- (+ x x)) 
;; theoretically desugars to: (double <- (x -> (+ x x)))
```

todo:
------
1. Create namespace for datatypes and traits (including `->`)
1. Add datatypes
1. Add an `if`
1. Imports/modules
1. pattern matching
1. Work on a compiler?
1. Add atoms? `(:hi)`


(Eventual) Spec for Datatypes
------------
``` lisp
;; type fn literal is only allowed in data/trait declarations 
;; and their respective constructors/signatures
(-> Nat (-> Nat Nat)) ;; type of +

;; type constructors are type function bindings to a new closed type
(data Bool
      (T <- Bool)
      (F <- Bool))

;; data polymorphism, captured vars are types 
;; (because constructors are type functions (no dependent types))
(data (Hmm Type)
      (Nah <- (Hmm a))
      ((Yah a) <- (Hmm a)))

;; type and constructor names may clash
(data (, Type Type)
      ((, a b) <- (, a b)))

;; data is recursive
(data (List Type)
      (Nil <- (List a))
      ((Cons a (List a)) <- (List a)))

;; data is a GADT
(data (AST Type)
      ((Lit a)                   <- (AST a))
      ((Add (AST Nat) (AST Nat)) <- (AST Nat))
      ((Eql (AST a) (AST a))     <- (AST Bool)))
```


On Type Aliases
----------
Ideally, this is totally unnecessary. Currently it's unusable.

Right now there are no type hints. I think if binding level typings are allowed, we can add this.
``` lisp
(type (AggFn a) (-> a (-> a a)))
((AggFn a) <~ (-> a (-> a a))) ;; mirrors inf type err msg T0 !~ T1
((AggFn a) :: (-> a (-> a a)))
((AggFn a) <-> (-> a (-> a a)))
```

to brainstorm: 
---------------
1. trait declarations `(trait (Num a) ((+ a a) <- a))`
1. kind syntax? `(=> (Num a) (=> (Show a) (a -> IO a)))`
1. data/trait variables (see haskell def of `(Monad m)`)