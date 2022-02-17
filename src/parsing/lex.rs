use std::{iter::Peekable, str::Chars};

pub struct Source<'a> {
    txt: Peekable<Chars<'a>>,
    name: Option<&'a String>
}

pub enum SToken {
    Word(String),
    SExp(Vec<SToken>),
}

pub enum LexError {

}

pub type LexResult<T> = Result<T, LexError>;

fn valid_token_char(c: &char) -> bool {
    !(*c == '(' || *c == ')' || c.is_whitespace())
}

fn is_whitespace(c: &char) -> bool { c.is_whitespace() }

impl<'a> Source<'a> {
    pub fn lex(&mut self) -> LexResult<Vec<SToken>> {
        
        unimplemented!()
    }

    fn lex_stoken(&mut self) -> LexResult<SToken> {
        unimplemented!()
    }

    fn next_stoken(&mut self) -> Option<SToken> {
        match self.txt.next() {
            None => None,
            Some('(') => {
                self.skip_whitespace();
                Some(LP)
            },
            Some(')') => {
                self.skip_whitespace();
                Some(RP)
            },
            Some(c) => {
                let mut w = String::new();
                w.push(c);
                w.extend(crate::take_while(&mut self.txt, valid_token_char));

            }
        }
    }

    fn skip_whitespace(&mut self) {
        crate::skip_while(&mut self.txt, is_whitespace);
    }
}