use std::{str::Chars, fmt::Display};

use super::FilePos;

#[derive(Debug)]
pub struct Source<'a> {
    src: &'a String,
    txt: Chars<'a>,
    pos: FilePos<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Quote,
    Word(String),
    SExp(Vec<Token>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum LexErrorType<'a> {
    TooManyClosing,
    Unclosed(FilePos<'a>),
}

impl<'a> LexErrorType<'a> {
    pub fn col_arrow(&self, file_pos: &FilePos) -> String {
        let file_pos = match &self {
            Self::TooManyClosing => file_pos,
            Self::Unclosed(file_pos) => file_pos
        };

        let mut out = String::new();
        out.extend((0..(file_pos.col - 2)).map(|_| ' '));
        out.push('^');
        out
    }

    pub fn name(&self) -> String {
        match self {
            Self::TooManyClosing => format!("Extra Closing Parenthesis"),
            Self::Unclosed(_) => format!("Unclosed S-Expression")
        }
    }
}

#[derive(Debug)]
pub struct LexError<'a> {
    src: Source<'a>,
    tipe: LexErrorType<'a>,
}

impl<'a> Display for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_indicator = format!(" {} ", self.src.pos.row);
        let margin: String = (0..line_indicator.len()).map(|_| ' ').collect();
        write!(f, "error: {} at {}\n{}| {}\n{}  {}", 
            self.tipe.name(), 
            self.src.pos, 
            line_indicator, 
            self.src.current_line(),
            margin,
            self.tipe.col_arrow(&self.src.pos))
    }
}

pub type LexResult<'a, T> = Result<T, LexError<'a>>;

impl<'a> Source<'a> {
    pub fn new(src: &'a String, name: Option<&'a String>) -> Self {
        Self { src, txt: src.chars(), pos: FilePos::new(name) }
    }

    pub fn lex(mut self) -> LexResult<'a, Vec<Token>> {
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
                Some('\'') => stack.push_token(Token::Quote),
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

    fn current_line(&self) -> String {
        String::from(self.src.lines().nth(self.pos.row - 1).unwrap())
    }
}

struct LexStack<'a> {
    sexp_stack: Vec<(FilePos<'a>, Vec<Token>)>,
    finished: Vec<Token>,
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
        let mut finished = last_sexp.ok_or(LexErrorType::TooManyClosing)?.1;

        if self.curr_word.len() > 0 {
            finished.push(self.dump_curr());
        }

        self.push_token(Token::SExp(finished));
        Ok(self)
    }

    fn push_token(&mut self, t: Token) {
        self.try_push_word();
        self.sexp_stack
            .last_mut()
            .map(|(_, v)| v)
            .unwrap_or(&mut self.finished)
            .push(t)
    }

    fn dump_curr(&mut self) -> Token {
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

    fn finish(mut self) -> Result<Vec<Token>, LexErrorType<'a>> {
        if let Some((file_pos, _)) = self.sexp_stack.pop() {
            Err(LexErrorType::Unclosed(file_pos))
        } else {
            Ok(self.finished)
        }
    }
}
