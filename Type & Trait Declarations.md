# Type & Trait Declarations

## Symbol Contexts

``` lisp
;; f, x, 3 all live in standard context
((f x) <- (f (+ x 3))) 
;; | ()

;;  ->, Types live in a different context - the type context
(-> Bool (-> Unit Int)) 
;; ** Unknown Symbol "->"

(data (Maybe Type)      ;; "Type" is only allowed in kind-context
                        ;; "Maybe" is immediately exposed in type-ctxt
    ((Just a)           ;; "Just" constructor name is not a type, but "a" is
        <- (Maybe a))   ;; "Maybe" is an error on its own, need "Maybe a"
    (Nothing            ;; "Nothing" is a constructor
        <- (Maybe a)))  ;; "a" is unbound, but that is ok!

;; type constructors are exposed to the standard context
Nothing
;; | (Maybe a)

(Just 5)
;; | (Maybe Nat)

(Maybe Nat)
;; ** Unknown Symbol "Maybe"
```
