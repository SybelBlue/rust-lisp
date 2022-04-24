pub mod parsing;
pub mod exprs;
pub mod errors;

#[cfg(test)]
mod tests {
    mod types {
        use crate::{exprs::typing::*, parsing::sources::Source};

        fn type_test<'a>(s: &'a str) -> Type {
            use crate::exprs::typing::contexts::TypeContext;
            use crate::parsing::parse_tokens;

            let src = Source::Anon(s);
            let ref mut buf = String::new();
            let ts = src.lex(buf).unwrap();
            let es = parse_tokens(ts).unwrap();
            crate::exprs::typing::checking::type_expr(&es[0], TypeContext::new()).unwrap().0
        }
        
        macro_rules! assert_fmt_eq {
            ($a:expr, $b:expr) => {
                assert_eq!(format!("{}", $a), format!("{}", $b));
            };
        }

        #[test]
        fn basic() {
            use crate::exprs::typing::Type::*;
            let fun = crate::exprs::typing::Type::fun;

            assert_eq!(Unit, type_test("()"));
            assert_eq!(Unit, type_test("(())"));
            assert_eq!(Nat, type_test("3"));
            assert_eq!(Nat, type_test("(3)"));
            assert_eq!(Nat, type_test("((3))"));
            
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test(r"+"));
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test(r"(+)"));
            
            // TODO: these guys            
            // Str,
            // Type,
            // Data(String),
            // Var(usize),
        }

        #[test]
        fn type_fn_basic() {
            use crate::exprs::typing::Type::*;

            assert_eq!(Type, type_test("Unit"));
            assert_eq!(Type, type_test("(Unit)"));
            assert_eq!(Type, type_test("Nat"));
            assert_eq!(Type, type_test("(Nat)"));
            assert_eq!(Type, type_test("((Nat))"));
            assert_eq!(Type, type_test("Type"));
        }

        #[test]
        fn lambdas() {
            use crate::exprs::typing::Type::*;
            let fun = crate::exprs::typing::Type::fun;
            
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test("(x -> (+ x))"));

            assert_fmt_eq!(fun(Var(1), Var(1)), type_test("(x -> x)"));
            assert_fmt_eq!(fun(fun(Nat, Var(1)), Var(1)), type_test("(f -> (f 3))"));

            assert_eq!(fun(fun(Nat, fun(Nat, Nat)), Nat), type_test("(f -> (f (f 1 2) (f 3 4)))"));
        }

        #[test]
        fn type_fn_lambdas() {
            use crate::exprs::typing::Type::*;
            let fun = crate::exprs::typing::Type::fun;

            assert_fmt_eq!(fun(fun(Type, Var(1)), Var(1)), type_test("(f -> (f Nat))"));
            assert_fmt_eq!(fun(fun(Type, fun(Unit, Type)), Type), type_test("(f -> (f (f Nat ()) ()))"));
        }

        #[test]
        fn aviary() {
            use crate::exprs::typing::Type::Var;
            let fun = crate::exprs::typing::Type::fun;

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
    }

    mod lexing {
        use crate::{exprs::SToken, parsing::sources::Source};

        #[test]
        fn basic() {
            use crate::parsing::lex::Token;
            
            #[derive(Debug, PartialEq, Eq)]
            enum QSW<'a> {
                A(bool),
                S(Vec<QSW<'a>>),
                W(&'a str)
            }
            use QSW::*;
            impl<'a> From<&'a Token<'a>> for QSW<'a> {
                fn from(t: &'a Token<'a>) -> Self {
                    match t {
                        Token::Word(s, _) => W(s.as_str()),
                        Token::SExp(SToken { body, ..}) => S(body.0.iter().map(QSW::from).collect()),
                        Token::Arrow(f, _) => A(*f),
                    }
                }
            }
    
            let src = Source::Anon("()\nhello\n(+ 12 34 53) (  ->    test\n\t\n\n (2 hi)  ) (x <- 4) (<- bad) (test0(test1)test-2(test3 -> test4)) (0 (1 (2 (3)) ((4) 5)) 6)");
            let test = vec!
                [ S(vec![])
                , W("hello")
                , S(vec![W("+"), W("12"), W("34"), W("53")])
                , S(vec![A(true), W("test"), S(vec![W("2"), W("hi")])])
                , S(vec![W("x"), A(false), W("4")])
                , S(vec![A(false), W("bad")])
                , S(vec![W("test0"), S(vec![W("test1")]), W("test-2"), S(vec![W("test3"), A(true), W("test4")])])
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
