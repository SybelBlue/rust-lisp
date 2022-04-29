# Import System


``` lisp
;; import namespace
(import Aviary)

;; import specific name
(import Typing.Contexts (exposing new Context))

;; import namespace as 
(import Huge.Namespace HN)

;; import excluding
(import Farm (excluding goose gopher))
```

*import* ::= `(` `import` `<Namespace>` *body* `)`

*body* ::= empty | `(` *body_type* `<Names>...` `)`

*body_type* ::= `exposing` | `excluding`