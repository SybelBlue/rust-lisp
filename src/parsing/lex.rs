use crate::exprs::SBody;

use super::{FilePos, lex_error::*};

#[derive(Debug)]
pub struct Source<'a> {
    pub src: &'a String,
    txt: std::str::Chars<'a>,
    pub(crate) pos: FilePos<'a>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Token<'a> {
    LamSlash(FilePos<'a>),
    Word(String),
    SExp(SBody<'a, Token<'a>>),
}

pub type LexResult<'a, T> = Result<T, LexError<'a>>;

impl<'a> Source<'a> {
    pub fn new(src: &'a String, name: Option<&'a String>) -> Self {
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
                        Err(tipe) => return Err(LexError { tipe, src: self })
                    },
                Some('\\') => stack.push_token(Token::LamSlash(self.pos.clone())),
                Some(ch) => stack.push_char(ch),
            }
        }

        stack.try_push_word();

        stack.finish()
            .map_err(|tipe| LexError { tipe, src: self })
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

    fn close_sexp(mut self) -> Result<Self, LexErrorType<'a>> {
        let last_sexp = self.sexp_stack.pop();
        let (start, mut body) = last_sexp.ok_or(LexErrorType::TooManyClosing)?;

        if self.curr_word.len() > 0 {
            body.push(self.dump_curr());
        }

        self.push_token(Token::SExp(SBody { start, body }));
        Ok(self)
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

    fn finish(mut self) -> Result<Vec<Token<'a>>, LexErrorType<'a>> {
        if let Some((file_pos, _)) = self.sexp_stack.pop() {
            Err(LexErrorType::Unclosed(file_pos))
        } else {
            Ok(self.finished)
        }
    }
}
