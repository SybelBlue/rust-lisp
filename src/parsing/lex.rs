use std::str::Chars;

use crate::{exprs::{SToken, SExp}, errors::{LexError, LexResult, LexErrorBody, Loc}};

use super::FilePos;

#[derive(Debug, Clone)]
pub enum Token<'a> {
    Arrow(bool, FilePos<'a>),
    Word(String, FilePos<'a>),
    SExp(SToken<'a, Token<'a>>),
}

#[derive(Debug)]
pub(crate) struct SourceIter<'a> {
    pub(crate) txt: Chars<'a>,
    pub(crate) pos: FilePos<'a>,
}

impl<'a> SourceIter<'a> {
    pub(crate) fn error(self, body: LexErrorBody<'a>) -> LexError<'a> {
        Loc::new(self.pos, body)
    }

    pub(crate) fn lex(mut self) -> LexResult<'a, Vec<Token<'a>>> {
        let mut stack = LexStack::new(self.pos.clone());

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
                        Err(body) => return Err(self.error(body))
                    },
                Some(ch) => stack.push_char(ch, &self.pos),
            }
        }

        stack.try_push_word();

        stack.finish()
            .map_err(|body| self.error(body))
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
}

struct LexStack<'a> {
    sexp_stack: Vec<(FilePos<'a>, Vec<Token<'a>>)>,
    finished: Vec<Token<'a>>,
    curr_word: String,
    curr_word_start: FilePos<'a>,
}

impl<'a> LexStack<'a> {
    fn new(start_pos: FilePos<'a>) -> Self {
        Self {
            sexp_stack: Vec::with_capacity(8),      // 8 deep nested sexp
            finished: Vec::with_capacity(50),       // 50 tokens on top level
            curr_word: String::with_capacity(10),   // 10 char words
            curr_word_start: start_pos,   
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

        self.push_token(Token::SExp(SToken { pos: start, body: SExp(body) }));
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
        let out = match self.curr_word.as_bytes() {
            b"->" => Token::Arrow(true, self.curr_word_start.clone()),
            b"<-" => Token::Arrow(false, self.curr_word_start.clone()),
            _ => Token::Word(self.curr_word.clone(), self.curr_word_start.clone()),
        };
        self.curr_word.clear();
        out
    }

    fn push_char(&mut self, ch: char, curr_pos: &FilePos<'a>) { 
        if self.curr_word.is_empty() {
            self.curr_word_start = curr_pos.clone();
        }
        self.curr_word.push(ch);
    }

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
