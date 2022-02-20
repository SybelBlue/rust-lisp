pub mod parsing;

#[cfg(test)]
mod tests {
    #[test]
    fn basic() {
        use crate::parsing::lex::{Token::{self, *}, Source};
        fn word(s: &str) -> Token { Word(String::from(s)) }

        let src = format!("()\nhello\n(+ 12 34 53) (    test\n\t\n\n 2 ' hi) (test0(test1)test-2'(test3)) (0 (1 (2 (3)) ((4) 5)) 6)");
        let test = vec!
            [ SExp(vec![])
            , word("hello")
            , SExp(vec![word("+"), word("12"), word("34"), word("53")])
            , SExp(vec![word("test"), word("2"), Quote, word("hi")])
            , SExp(vec![word("test0"), SExp(vec![word("test1")]), word("test-2"), Quote, SExp(vec![word("test3")])])
            , SExp(vec![word("0")
                , SExp(vec![word("1")
                    , SExp(vec![word("2")
                        , SExp(vec![word("3")])])
                    , SExp(vec![SExp(vec![word("4")])
                        , word("5")])
                    ])
                , word("6")])];
        
        
        let mut source = Source::new(&src, None);
        let ts = source.lex()
            .map_err(|e| format!("lexing failed with Error {:?}", e))
            .unwrap();

        assert_eq!(test.len(), ts.len(), "lex returned wrong number of tokens {} (not {})", ts.len(), test.len());

        for (s, t) in ts.into_iter().zip(test) {
            assert_eq!(s, t);
        }
    }
}
