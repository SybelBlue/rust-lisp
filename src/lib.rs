pub mod parsing;
pub mod interpreting;
pub mod exprs;

#[cfg(test)]
mod tests {
    mod types {
        use crate::exprs::types::*;

        fn type_test<'a>(s: &'a str) -> Type {
            use crate::exprs::contexts::TypeContext;
            use crate::parsing::{lex::Source, parse_tokens};

            let src = Source::new(&s, None);
            let ts = src.lex().unwrap();
            let es = parse_tokens(ts).unwrap();
            type_expr(&es[0], TypeContext::new()).unwrap().0
        }
        
        macro_rules! assert_fmt_eq {
            ($a:expr, $b:expr) => {
                assert_eq!(format!("{}", $a), format!("{}", $b));
            };
        }

        #[test]
        fn basic() {
            use crate::exprs::types::Type::*;
            let fun = crate::exprs::types::Type::fun;

            assert_eq!(Unit, type_test("()"));
            assert_eq!(Unit, type_test("(())"));
            assert_eq!(Nat, type_test("3"));
            assert_eq!(Nat, type_test("(3)"));
            assert_eq!(Nat, type_test("(3)"));
            
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test(r"+"));
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test(r"(+)"));
            
            // TODO: these guys            
            // Str,
            // Type,
            // Data(String),
            // Var(usize),
        }

        #[test]
        fn lambdas() {
            use crate::exprs::types::Type::*;
            let fun = crate::exprs::types::Type::fun;
            
            assert_eq!(fun(Nat, fun(Nat, Nat)), type_test(r"(\x (+ x))"));

            assert_eq!(format!("{}", fun(Var(1), Var(1))), format!("{}", type_test(r"(\x x)")));
            assert_eq!(format!("{}", fun(fun(Nat, Var(1)), Var(1))), format!("{}", type_test(r"(\f (f 3))")));
        }

        #[test]
        fn aviary() {
            use crate::exprs::types::Type::Var;
            let fun = crate::exprs::types::Type::fun;

            // kestrel (const)
            assert_fmt_eq!(fun(Var(2), fun(Var(1), Var(2))), type_test(r"(\x (\_ x))"));
            
            // psi (on)
            assert_fmt_eq!(
                fun(fun(Var(2), fun(Var(2), Var(3))), 
                    fun(fun(Var(1), Var(2)), 
                    fun(Var(1), 
                    fun(Var(1)
                    , Var(3))))), type_test(r"(\(f g x y) (f (g x) (g y)))"));
            
            // bluebird (.)
            assert_fmt_eq!(
                fun(fun(Var(2), Var(3)), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test(r"(\(g f x) (g (f x)))"));
            
            // cardinal (flip)
            assert_fmt_eq!(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(Var(2), fun(Var(1), Var(3)))),
                type_test(r"(\(f b a) (f a b))"));
            
            // applicator ($)
            assert_fmt_eq!(
                fun(fun(Var(1), Var(2)), fun(Var(1), Var(2))),
                type_test(r"(\(f a) (f a))"));
            
            // starling (<*> over (->))
            assert_fmt_eq!(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test(r"(\(fabc gab a) (fabc a (gab a)))"));
            
            // pheonix/starling' (liftA2/liftM2 over (->))
            assert_fmt_eq!(
                fun(fun(Var(2), fun(Var(3), Var(4))), fun(fun(Var(1), Var(2)), fun(fun(Var(1), Var(3)), fun(Var(1), Var(4))))),
                type_test(r"(\(fbcd gab hac a) (fbcd (gab a) (hac a)))"));
        }
    }

    mod lexing {
        use crate::exprs::SBody;

        #[test]
        fn basic() {
            use crate::parsing::lex::{Token, Source};
            
            #[derive(Debug, PartialEq, Eq)]
            enum QSW<'a> {
                L,
                S(Vec<QSW<'a>>),
                W(&'a str)
            }
            use QSW::*;
            impl<'a> From<&'a Token<'a>> for QSW<'a> {
                fn from(t: &'a Token<'a>) -> Self {
                    match t {
                        Token::LamSlash(_) => L,
                        Token::Word(s) => W(s.as_str()),
                        Token::SExp(SBody { body, ..}) => S(body.into_iter().map(QSW::from).collect()),
                    }
                }
            }
    
            let src = format!("()\nhello\n(+ 12 34 53) (  \\    test\n\t\n\n (2 hi)  ) (test0(test1)test-2(\\test3 test4)) (0 (1 (2 (3)) ((4) 5)) 6)");
            let test = vec!
                [ S(vec![])
                , W("hello")
                , S(vec![W("+"), W("12"), W("34"), W("53")])
                , S(vec![L, W("test"), S(vec![W("2"), W("hi")])])
                , S(vec![W("test0"), S(vec![W("test1")]), W("test-2"), S(vec![L, W("test3"), W("test4")])])
                , S(vec![W("0")
                    , S(vec![W("1")
                        , S(vec![W("2")
                            , S(vec![W("3")])])
                        , S(vec![S(vec![W("4")])
                            , W("5")])
                        ])
                    , W("6")])];
            
            
            let source = Source::new(&src, None);
            let ts = source.lex()
                .map_err(|e| println!("lexing failed with Error {}", e))
                .unwrap();
    
            assert_eq!(test.len(), ts.len(), "lex returned wrong number of tokens {} (not {})", ts.len(), test.len());
    
            for (s, t) in ts.iter().map(QSW::from).zip(test) {
                assert_eq!(s, t);
            }
        }

    }
}
