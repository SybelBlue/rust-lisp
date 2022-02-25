pub mod parsing;
pub mod interpreting;
pub mod exprs;

#[cfg(test)]
mod tests {
    mod types {
        // use crate::types::*;
        #[test]
        fn basic() {
            // let src = format!("()\nhello\n(+ 12 34 53) (    test\n\t\n\n 2 ' hi) (test0(test1)test-2'(test3)) (0 (1 (2 (3)) ((4) 5)) 6)");
            // let source = crate::parsing::lex::Source::new(&src, None);
            // let ts = source.lex()
            //     .map_err(|e| println!("lexing failed with Error {}", e))
            //     .unwrap();
            // let es = crate::parsing::parse::parse_tokens(ts)
            //     .map_err(|e| println!("parsing failed with Error {:?}", e))
            //     .unwrap();
            // let mut ctxt = crate::types::TypeContext::new();
            // let t = crate::types::type_expr(e, &mut ctxt)
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
