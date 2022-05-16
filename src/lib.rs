pub mod parsing;
pub mod data;
pub mod values;
pub mod exprs;
pub mod stmts;
pub mod errors;
pub mod typing;
pub mod repl;

#[cfg(test)]
mod tests {
    mod types {
        use crate::typing::contexts::Context;
        use crate::{typing::*, parsing::sources::Source};
        use crate::typing::{scheme::Scheme, Type::*};

        fn type_test<'a>(s: &'a str) -> Type {
            type_test_all(s).pop().unwrap()
        }

        fn type_test_all<'a>(s: &'a str) -> Vec<Type> {
            type_test_all_with(s, Context::new(), false).1
        }

        fn type_test_all_with<'a>(s: &'a str, ctxt: Context, err_ok: bool) -> (Context, Vec<Type>) {
            use crate::typing::infer::infer;

            let src = Source::Anon(s);
            let ref mut buf = String::new();
            let ts = src.lex(buf).unwrap();
            let ss = crate::parsing::parse(ts).unwrap();
            let (ctxt, res) = infer(ctxt, &ss);
            match res {
                Err(e) =>
                    if err_ok {
                        (ctxt, Vec::with_capacity(0))
                    } else {
                        panic!("{}", e)
                    },
                Ok(scs) =>
                    (ctxt,
                        scs
                            .into_iter()
                            .map(|v| v.tipe)
                            .collect())
            }
        }

        fn agg(t: Type) -> Type {
            Type::fun(t.clone(), Type::fun(t.clone(), t))
        }
        
        fn assert_type_eq(a: Type, b: Type) {
            let a = Scheme::concrete(a);
            let b = Scheme::concrete(b);
            assert_eq!(
                a.normalize().tipe, 
                b.normalize().tipe,
                "{} !~ {}",
                &a.tipe,
                &b.tipe
            );
        }

        #[test]
        fn basic() {
            use crate::typing::Type;

            assert_eq!(Type::unit(), type_test("()"));
            assert_eq!(Type::unit(), type_test("(())"));
            assert_eq!(Type::nat(), type_test("3"));
            assert_eq!(Type::nat(), type_test("(3)"));
            assert_eq!(Type::nat(), type_test("((3))"));
            
            assert_eq!(agg(Type::nat()), type_test(r"+"));
            assert_eq!(agg(Type::nat()), type_test(r"(+)"));
        }

        #[test]
        fn lambdas() {
            let fun = crate::typing::Type::fun;
            
            assert_eq!(agg(Type::nat()), type_test("(x -> (+ x))"));

            assert_type_eq(fun(Var(1), Var(1)), type_test("(x -> x)"));
            assert_type_eq(fun(fun(Type::nat(), Var(1)), Var(1)), type_test("(f -> (f 3))"));

            assert_eq!(fun(agg(Type::nat()), Type::nat()), type_test("(f -> (f (f 1 2) (f 3 4)))"));
        }

        #[test]
        fn basic_binds() {
            assert_eq!(Type::unit(), type_test("(unit <- ())"));
            assert_eq!(Type::unit(), type_test("(unit <- (()))"));
            assert_eq!(Type::unit(), type_test("(unit <- (())) unit"));
            assert_eq!(Type::nat(), type_test("(x <- 3)"));
            assert_eq!(Type::nat(), type_test("(x <- (3))"));
            assert_eq!(Type::nat(), type_test("(x <- 3) x"));
            assert_eq!(Type::fun(Type::nat(), Type::nat()), type_test("((double x) <- (+ x x))"));
            assert_eq!(Type::nat(), type_test("(x <- 3) ((double x) <- (+ x x)) (double x)"));
        }

        #[test]
        fn aviary() {
            let fun = crate::typing::Type::fun;

            // kestrel (const)
            assert_type_eq(fun(Var(2), fun(Var(1), Var(2))), type_test("(x -> (_ -> x))"));
            
            // psi (on)
            assert_type_eq(
                fun(fun(Var(2), fun(Var(2), Var(3))), 
                    fun(fun(Var(1), Var(2)), 
                    fun(Var(1), 
                    fun(Var(1)
                    , Var(3))))), type_test("((f g x y) -> (f (g x) (g y)))"));
            
            // bluebird (.)
            assert_type_eq(
                fun(fun(Var(2), Var(3)), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((g f x) -> (g (f x)))"));
            
            // cardinal (flip)
            assert_type_eq(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(Var(2), fun(Var(1), Var(3)))),
                type_test("((f b a) -> (f a b))"));
            
            // applicator ($)
            assert_type_eq(
                fun(fun(Var(1), Var(2)), fun(Var(1), Var(2))),
                type_test("((f a) -> (f a))"));
            
            // starling (<*> over (->))
            assert_type_eq(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((fabc gab a) -> (fabc a (gab a)))"));
            
            // pheonix/starling' (liftA2/liftM2 over (->))
            assert_type_eq(
                fun(fun(Var(2), fun(Var(3), Var(4))), fun(fun(Var(1), Var(2)), fun(fun(Var(1), Var(3)), fun(Var(1), Var(4))))),
                type_test("((fbcd gab hac a) -> (fbcd (gab a) (hac a)))"));
        }

        #[test]
        fn named_aviary() {
            use crate::typing::Type::Var;
            let fun = crate::typing::Type::fun;

            assert_type_eq(fun(Var(2), fun(Var(1), Var(2))), type_test("((kestrel x) <- (_ -> x))"));
            
            assert_type_eq(
                fun(fun(Var(2), fun(Var(2), Var(3))), 
                    fun(fun(Var(1), Var(2)), 
                    fun(Var(1), 
                    fun(Var(1)
                    , Var(3))))), type_test("((on f g x y) <- (f (g x) (g y)))"));
            
            assert_type_eq(
                fun(fun(Var(2), Var(3)), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((bluebird g f x) <- (g (f x)))"));
            
            assert_type_eq(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(Var(2), fun(Var(1), Var(3)))),
                type_test("((cardinal f b a) <- (f a b))"));
            
            assert_type_eq(
                fun(fun(Var(1), Var(2)), fun(Var(1), Var(2))),
                type_test("(($ f a) <- (f a))"));
            
            assert_type_eq(
                fun(fun(Var(1), fun(Var(2), Var(3))), fun(fun(Var(1), Var(2)), fun(Var(1), Var(3)))),
                type_test("((starling fabc gab a) <- (fabc a (gab a)))"));
            
            assert_type_eq(
                fun(fun(Var(2), fun(Var(3), Var(4))), fun(fun(Var(1), Var(2)), fun(fun(Var(1), Var(3)), fun(Var(1), Var(4))))),
                type_test("((phoenix fbcd gab hac a) <- (fbcd (gab a) (hac a)))"));
        }

        #[test]
        fn recursive() {
            assert_type_eq(
                Type::fun(Type::nat(), Type::nat()), 
                type_test("((succ-inf n) <- (+ 1 (succ-inf (+ n 1))))"));
        }

        #[test]
        fn mod_parapoly() {            
            let types = type_test_all("\
            ((id x) <- x)
            ((bux z) <- 5)
            ((foo x y) <- (+ (id (bux 3)) (+ x (bux (id ())))))");
            assert_eq!(3, types.len());
            println!("{:?}", &types);
            vec![ Type::fun(Type::Var(0), Type::Var(0))
                , Type::fun(Type::Var(0), Type::nat())
                , Type::fun(Type::nat(), Type::fun(Type::Var(0), Type::nat()))
                ]
                .into_iter()
                .zip(types)
                .for_each(|(e, g)| assert_type_eq(e, g));
        }

        #[test]
        fn context_edits() {
            let ctxt = Context::new();
            let n = ctxt.get_varnames().count();
            let (ctxt, ts) = 
                type_test_all_with("(dont_bind_bc_type_err <- 3) (ord dont_bind_bc_type_err)", ctxt, true);
            assert_eq!(ts, Vec::new());
            assert_eq!(ctxt.get_varnames().count(), n);

            let (ctxt, ts) = 
                type_test_all_with("(z <- 3)", ctxt, false);
            assert_eq!(ts, vec![Type::nat()]);
            assert_eq!(ctxt.get_varnames().count(), n + 1);

            let (ctxt, ts) = 
                type_test_all_with("((test_argname_mask z) <- (ord z))", ctxt, false);
            assert_eq!(ts, vec![Type::fun(Type::char(), Type::nat())]);
            assert_eq!(ctxt.get_varnames().count(), n + 2);

            let (ctxt, ts) = 
                type_test_all_with("(test_argname_mask z)", ctxt, true);
            assert_eq!(ts, vec![]);
            assert_eq!(ctxt.get_varnames().count(), n + 2);
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
                W(&'a str),
                L(char)
            }
            use QSW::*;
            use Keyword::*;
            impl<'a> From<&'a Token<'a>> for QSW<'a> {
                fn from(t: &'a Token<'a>) -> Self {
                    match &t.body {
                        TokenBody::Word(s) => W(s.as_str()),
                        TokenBody::SExp(body) => S(body.iter().map(QSW::from).collect()),
                        TokenBody::Keyword(kw) => K(*kw),
                        TokenBody::Literal(c) => L(*c),
                    }
                }
            }
    
            let src = Source::Anon("()\nhello\n(+ 12 34 53) (  ->    
                test\n\t\n\n (2 hi)  ) (x <- 4) 
                (<- bad) 
                ('h' '\\n' 'ok hi't'here ' '')
                (test0(test1)test-2(test3 -> test4)) 
                (0 (1 (2 (3)) ((4) 5)) 6)");
            let test = vec!
                [ S(vec![])
                , W("hello")
                , S(vec![W("+"), W("12"), W("34"), W("53")])
                , S(vec![K(Arrow), W("test"), S(vec![W("2"), W("hi")])])
                , S(vec![W("x"), K(Backarrow), W("4")])
                , S(vec![K(Backarrow), W("bad")])
                , S(vec![L('h'), L('\n'), W("'ok"), W("hi't'here"), L(' '), W("'")])
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
