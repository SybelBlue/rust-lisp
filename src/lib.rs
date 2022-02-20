pub mod parsing;

#[cfg(test)]
mod tests {
    #[test]
    fn basic() {
        use crate::parsing::lex::{Token::{self, *}, Source};

        let src = format!("()\nhello\n(+ 12 34 53) (    test\n\t\n\n 2 ) (0 (1 (2 (3)) ((4) 5)) 6)");
        let mut source = Source::new(&src, None);

        fn word(s: &str) -> Token { Word(String::from(s)) }

        assert_eq!(
            source.lex(), 
            Ok(vec!
                [ SExp(vec![])
                , word("hello")
                , SExp(vec![word("+"), word("12"), word("34"), word("53")])
                , SExp(vec![word("test"), word("2")])
                , SExp(vec![word("0")
                    , SExp(vec![word("1")
                        , SExp(vec![word("2")
                            , SExp(vec![word("3")])])
                        , SExp(vec![SExp(vec![word("4")])
                            , word("5")])
                        ])
                    , word("6")])]))
    }
}
