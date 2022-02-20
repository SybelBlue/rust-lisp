use std::{iter::Peekable, str::Chars};

#[derive(Debug)]
pub struct Source<'a> {
    pub(crate) txt: Peekable<Chars<'a>>,
    pub name: Option<&'a String>
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    SExp(Vec<Token>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LexError {
    TooManyClosing,
    Unclosed
}

pub type LexResult<T> = Result<T, LexError>;

impl<'a> Source<'a> {
    pub fn new(source: &'a String, name: Option<&'a String>) -> Self {
        Self {
            txt: source.chars().peekable(),
            name,
        }
    }

    pub fn lex(&mut self) -> LexResult<Vec<Token>> {
        let mut out = Vec::with_capacity(50);
        let mut sexp_stack: Vec<Vec<Token>> = Vec::with_capacity(6);
        let mut curr = String::with_capacity(10);

        fn dump_curr(curr: &mut String) -> Token {
            let out = Token::Word(curr.clone());
            curr.clear();
            out
        }

        loop {
            match self.txt.next() {
                None => break,
                Some('(') => sexp_stack.push(Vec::with_capacity(4)),
                Some(')') => {
                    let mut finished = sexp_stack.pop().ok_or(LexError::TooManyClosing)?;
                    
                    if !curr.is_empty() {
                        finished.push(dump_curr(&mut curr));
                    }
                    
                    sexp_stack.last_mut().unwrap_or(&mut out).push(Token::SExp(finished));
                },
                Some(ch) if ch.is_whitespace() => 
                    sexp_stack.last_mut().unwrap_or(&mut out).push(dump_curr(&mut curr)),
                Some(ch) => {
                    curr.push(ch);
                    continue;
                }
            }
            self.skip_whitespace();
        }

        if !curr.is_empty() {
            out.push(Token::Word(curr));
        }

        if sexp_stack.is_empty() {
            Ok(out)
        } else {
            Err(LexError::Unclosed)
        }
    }

    fn skip_whitespace(&mut self) {
        while self.txt.next_if(|c| c.is_whitespace()).is_some() {}
    }
}
