use crate::exprs::SBody;

use super::{FilePos, lex_error::*};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Token<'a> {
    LamSlash(FilePos<'a>),
    Word(String),
    SExp(SBody<'a, Token<'a>>),
}

impl<'a> Token<'a> {
    pub fn get_symbols(&'a self) -> Vec<String> {
        let mut symbols = Vec::new();
        let mut to_check = vec![self];
        while let Some(next) = to_check.pop() {
            match next {
                Token::LamSlash(_) => {},
                Token::Word(w) => symbols.push(w.clone()),
                Token::SExp(sbody) => to_check.extend(&sbody.body),
            }
        }
        symbols
    }
}

pub type LexResult<'a, T> = Result<T, LexError<'a>>;

#[derive(Debug)]
pub struct Source<'a> {
    pub src: &'a str,
    txt: std::str::Chars<'a>,
    pub(crate) pos: FilePos<'a>,
}

impl<'a> Source<'a> {
    pub fn new(src: &'a str, name: Option<&'a String>) -> Self {
        Self { src, txt: src.chars(), pos: FilePos::new(name) }
    }

    pub fn lex(mut self) -> LexResult<'a, Vec<Token<'a>>> {
        let mut stack = LexStack::new();

        loop {
            let (skipped, next) = self.advance();
            
            if skipped {
                stack.try_push_word();
            }
            
            match next {
                None => break,
                Some('(') => stack.open_sexp(self.pos.clone()),
                Some(')') => 
                    match stack.close_sexp() {
                        Ok(st) => stack = st,
                        Err(tipe) => return Err(LexError { body: tipe, src: self })
                    },
                Some('\\') => 
                    match stack.push_lambda(self.pos.clone()) {
                        Ok(st) => stack = st,
                        Err(tipe) => return Err(LexError { body: tipe, src: self })
                    },
                Some(ch) => stack.push_char(ch),
            }
        }

        stack.try_push_word();

        stack.finish()
            .map_err(|tipe| LexError { body: tipe, src: self })
    }

    fn advance(&mut self) -> (bool, Option<char>) {
        let n = self.txt.next();
        self.pos.advance(&n);
        if !matches!(n, Some(c) if c.is_whitespace()) {
            return (false, n);
        }
        loop {
            let n = self.txt.next();
            self.pos.advance(&n);
            if !matches!(n, Some(c) if c.is_whitespace()) {
                return (true, n);
            }
        }
    }

    pub(crate) fn current_line(&self) -> String {
        String::from(self.src.lines().nth(self.pos.row - 1).unwrap())
    }
}

struct LexStack<'a> {
    sexp_stack: Vec<(FilePos<'a>, Vec<Token<'a>>)>,
    finished: Vec<Token<'a>>,
    curr_word: String,
}

impl<'a> LexStack<'a> {
    fn new() -> Self {
        Self {
            sexp_stack: Vec::with_capacity(8),      // 8 deep nested sexp
            finished: Vec::with_capacity(50),       // 50 tokens on top level
            curr_word: String::with_capacity(10),   // 10 char words
        }
    }

    fn open_sexp(&mut self, file_pos: FilePos<'a>) {
        self.try_push_word();
        self.sexp_stack.push((file_pos, Vec::with_capacity(4))) // 4 long sexp
    }

    fn close_sexp(mut self) -> Result<Self, LexErrorBody<'a>> {
        let last_sexp = self.sexp_stack.pop();
        let (start, mut body) = last_sexp.ok_or(LexErrorBody::TooManyClosing)?;

        if self.curr_word.len() > 0 {
            body.push(self.dump_curr());
        }

        self.push_token(Token::SExp(SBody { start, body }));
        Ok(self)
    }

    fn push_lambda(mut self, fp: FilePos<'a>) -> Result<Self, LexErrorBody<'a>> {
        match self.sexp_stack.last() {
            None => Err(LexErrorBody::StartingLambda(fp)),
            Some((_, s)) if !s.is_empty() => 
                Err(LexErrorBody::StartingLambda(fp)),
            _ => {
                self.push_token(Token::LamSlash(fp));
        
                Ok(self)
            }
        }
    }

    fn push_token(&mut self, t: Token<'a>) {
        self.try_push_word();
        self.sexp_stack
            .last_mut()
            .map(|(_, v)| v)
            .unwrap_or(&mut self.finished)
            .push(t)
    }

    fn dump_curr(&mut self) -> Token<'a> {
        let out = Token::Word(self.curr_word.clone());
        self.curr_word.clear();
        out
    }

    fn push_char(&mut self, ch: char) { self.curr_word.push(ch) }

    fn try_push_word(&mut self) {
        if self.curr_word.len() > 0 {
            let t = self.dump_curr();
            self.push_token(t);
        }
    }

    fn finish(mut self) -> Result<Vec<Token<'a>>, LexErrorBody<'a>> {
        if let Some((file_pos, _)) = self.sexp_stack.pop() {
            Err(LexErrorBody::Unclosed(file_pos))
        } else {
            Ok(self.finished)
        }
    }
}
