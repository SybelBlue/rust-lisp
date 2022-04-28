pub mod parsing;
pub mod exprs;
pub mod errors;
pub mod typing;

#[cfg(test)]
mod tests {
    mod types {
        use crate::{typing::*, parsing::sources::Source};

        fn type_test<'a>(s: &'a str) -> Type {
            use crate::typing::contexts::TypeContext;

            let src = Source::Anon(s);
            let ref mut buf = String::new();
            let ts = src.lex(buf).unwrap();
            let ss = crate::parsing::parse(ts).unwrap();
            ss.iter().try_fold(
                (Type::Unit, TypeContext::new()), 
                |(_, ctxt), s| crate::typing::checking::type_stmt(&s, ctxt)
            ).unwrap().0

        }
        
        macro_rules! assert_fmt_eq {
            ($a:expr, $b:expr) => {
                assert_eq!(format!("{}", $a), format!("{}", $b));
            };
        }

        #[test]
        fn basic() {
            use crate::typing::Type::{*, self};
            let fun = Type::fun;

            assert_eq!(Unit, type_test("()"));
            assert_eq!(Unit, type_test("(())"));
            assert_eq!(Nat, type_test("3"));
            assert_eq!(Nat, type_test("(3)"));
            assert_eq!(Nat, type_test("((3))"));
            
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test(r"+"));
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test(r"(+)"));
        }

        #[test]
        fn lambdas() {
            use crate::typing::Type::*;
            let fun = crate::typing::Type::fun;
            
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test("(x -> (+ x))"));

            assert_fmt_eq!(fun(Var(1), Var(1)), type_test("(x -> x)"));
            assert_fmt_eq!(fun(fun(Nat, Var(1)), Var(1)), type_test("(f -> (f 3))"));

            assert_eq!(fun(fun(Nat, fun(Nat, Nat)), Nat), type_test("(f -> (f (f 1 2) (f 3 4)))"));
        }

        #[test]
        fn basic_binds() {
            use crate::typing::Type::{self, *};
            assert_eq!(Unit, type_test("(unit <- ())"));
            assert_eq!(Unit, type_test("(unit <- (()))"));
            assert_eq!(Unit, type_test("(unit <- (())) unit"));
            assert_eq!(Nat, type_test("(x <- 3)"));
            assert_eq!(Nat, type_test("(x <- (3))"));
            assert_eq!(Nat, type_test("(x <- 3) x"));
            assert_eq!(Type::fun(Nat, Nat), type_test("((double x) <- (+ x x))"));
            assert_eq!(Nat, type_test("(x <- 3) ((double x) <- (+ x x)) (double x)"));
        }

        #[test]
        fn aviary() {
            use crate::typing::Type::Var;
            let fun = crate::typing::Type::fun;

            // kestrel (const)
            assert_fmt_eq!(fun(Var(2), fun(Var(1), Var(2))), type_test("(x -> (_ -> x))"));
            
            // psi (on)
            assert_fmt_eq!(
                fun(fun(Var(2), fun(Var(2), Var(3))), 
                    fun(fun(Var(1), Var(2)), 
                    fun(Var(1), 
                    fun(Var(1)
                    , Var(3))))), type_test("((f g x y) -> (f (g x) (g y)))"));
            
            // bluebird (.)
            assert_fmt_eq!(
                fun(fun(Var(2), Var(3)), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((g f x) -> (g (f x)))"));
            
            // cardinal (flip)
            assert_fmt_eq!(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(Var(2), fun(Var(1), Var(3)))),
                type_test("((f b a) -> (f a b))"));
            
            // applicator ($)
            assert_fmt_eq!(
                fun(fun(Var(1), Var(2)), fun(Var(1), Var(2))),
                type_test("((f a) -> (f a))"));
            
            // starling (<*> over (->))
            assert_fmt_eq!(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((fabc gab a) -> (fabc a (gab a)))"));
            
            // pheonix/starling' (liftA2/liftM2 over (->))
            assert_fmt_eq!(
                fun(fun(Var(2), fun(Var(3), Var(4))), fun(fun(Var(1), Var(2)), fun(fun(Var(1), Var(3)), fun(Var(1), Var(4))))),
                type_test("((fbcd gab hac a) -> (fbcd (gab a) (hac a)))"));
        }

        #[test]
        fn named_aviary() {
            use crate::typing::Type::Var;
            let fun = crate::typing::Type::fun;

            assert_fmt_eq!(fun(Var(2), fun(Var(1), Var(2))), type_test("((kestrel x) <- (_ -> x))"));
            
            assert_fmt_eq!(
                fun(fun(Var(2), fun(Var(2), Var(3))), 
                    fun(fun(Var(1), Var(2)), 
                    fun(Var(1), 
                    fun(Var(1)
                    , Var(3))))), type_test("((on f g x y) <- (f (g x) (g y)))"));
            
            assert_fmt_eq!(
                fun(fun(Var(2), Var(3)), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((bluebird g f x) <- (g (f x)))"));
            
            assert_fmt_eq!(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(Var(2), fun(Var(1), Var(3)))),
                type_test("((cardinal f b a) <- (f a b))"));
            
            assert_fmt_eq!(
                fun(fun(Var(1), Var(2)), fun(Var(1), Var(2))),
                type_test("(($ f a) <- (f a))"));
            
            assert_fmt_eq!(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((starling fabc gab a) <- (fabc a (gab a)))"));
            
            assert_fmt_eq!(
                fun(fun(Var(2), fun(Var(3), Var(4))), fun(fun(Var(1), Var(2)), fun(fun(Var(1), Var(3)), fun(Var(1), Var(4))))),
                type_test("((phoenix fbcd gab hac a) <- (fbcd (gab a) (hac a)))"));
        }

        #[test]
        fn mod_test() {
            use crate::typing::contexts::TypeContext;

            let src = Source::Anon("\
            ((foo x) <- (baz (+ x y)))\
            (y <- 7)\
            ((baz x) <- (foo (foo (+ y x))))");
            let ref mut buf = String::new();
            let ts = src.lex(buf).unwrap();
            let ss = crate::parsing::parse(ts).unwrap();
            let (types, _) = crate::typing::checking::type_mod(&ss, TypeContext::new()).unwrap();
            let n_fn = Type::fun(Type::Nat, Type::Nat);
            assert_eq!(vec![n_fn.clone(), Type::Nat, n_fn], types);

        }
    }

    mod lexing {
        use crate::parsing::{sources::Source, lex::Keyword};

        #[test]
        fn basic() {
            use crate::parsing::lex::{Token, TokenBody};
            
            #[derive(Debug, PartialEq, Eq)]
            enum QSW<'a> {
                K(Keyword),
                S(Vec<QSW<'a>>),
                W(&'a str)
            }
            use QSW::*;
            use Keyword::*;
            impl<'a> From<&'a Token<'a>> for QSW<'a> {
                fn from(t: &'a Token<'a>) -> Self {
                    match &t.body {
                        TokenBody::Word(s) => W(s.as_str()),
                        TokenBody::SExp(body) => S(body.0.iter().map(QSW::from).collect()),
                        TokenBody::Keyword(kw) => K(*kw),
                    }
                }
            }
    
            let src = Source::Anon("()\nhello\n(+ 12 34 53) (  ->    
                test\n\t\n\n (2 hi)  ) (x <- 4) 
                (<- bad) 
                (test0(test1)test-2(test3 -> test4)) (0 (1 (2 (3)) ((4) 5)) 6)");
            let test = vec!
                [ S(vec![])
                , W("hello")
                , S(vec![W("+"), W("12"), W("34"), W("53")])
                , S(vec![K(Arrow), W("test"), S(vec![W("2"), W("hi")])])
                , S(vec![W("x"), K(Backarrow), W("4")])
                , S(vec![K(Backarrow), W("bad")])
                , S(vec![W("test0"), S(vec![W("test1")]), W("test-2"), S(vec![W("test3"), K(Arrow), W("test4")])])
                , S(vec![W("0")
                    , S(vec![W("1")
                        , S(vec![W("2")
                            , S(vec![W("3")])])
                        , S(vec![S(vec![W("4")])
                            , W("5")])
                        ])
                    , W("6")])];
            
            
            let ref mut buf = String::new();
            let ts = src.lex(buf)
                .map_err(|e| println!("lexing failed with Error {}", e))
                .unwrap();
    
            assert_eq!(test.len(), ts.len(), "lex returned wrong number of tokens {} (not {})", ts.len(), test.len());
    
            for (s, t) in ts.iter().map(QSW::from).zip(test) {
                assert_eq!(s, t);
            }
        }

    }
}
