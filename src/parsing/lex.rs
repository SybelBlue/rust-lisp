use std::str::Chars;

use crate::{errors::{LexError, LexResult, LexErrorBody}, parsing::sources::{FilePos, Loc}};

pub type Token<'a> = Loc<'a, TokenBody<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenBody<'a> {
    Keyword(Keyword),
    Word(String),
    Literal(char),
    SExp(Vec<Token<'a>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword { 
    Backarrow, 
    Arrow,
    Import,
    Data,
    Type,
}

impl Keyword {
    fn from(s: &str) -> Option<Self> {
        match s.as_bytes() {
            b"->" => Some(Self::Arrow),
            b"<-" => Some(Self::Backarrow),
            // b"import" => Some(Self::Import),
            b"data" => Some(Self::Data),
            b"Type" => Some(Self::Type),
            _ => None,
        }
    }
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Backarrow => "<-",
            Self::Arrow => "->",
            Self::Import => "import",
            Self::Data => "data",
            Self::Type => "Type",
        })
    }
}

#[derive(Debug)]
pub(crate) struct SourceIter<'a> {
    pub(crate) txt: Chars<'a>,
    pub(crate) pos: FilePos<'a>,
}

impl<'a> SourceIter<'a> {
    pub(crate) fn error(self, body: LexErrorBody<'a>) -> LexError<'a> {
        LexError::new(self.pos, body)
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
                Some('\'') if stack.curr_word.is_empty() => {
                    // must capture pos before self.try_lex_char_literal()!
                    let mut pos = self.pos.clone();
                    match self.try_lex_char_literal() {
                        Ok(c) =>
                            stack.push_token(Token { pos, body: TokenBody::Literal(c) } ),
                        Err(s) => {
                            for c in s.chars() {
                                match c {
                                    ')' => stack = stack.close_sexp().map_err(|body| LexError::new(pos.clone(), body))?,
                                    '(' => stack.open_sexp(pos.clone()),
                                    c if c.is_whitespace() => stack.try_push_word(),
                                    ch => stack.push_char(ch, &pos),
                                }
                                pos.advance(&Some(c));
                            }
                        }
                    }
                }
                Some(ch) => stack.push_char(ch, &self.pos),
            }
        }

        stack.try_push_word();

        stack.finish()
            .map_err(|body| self.error(body))
    }

    fn advance(&mut self) -> (bool, Option<char>) {
        match self.next() {
            Some(c) if c.is_whitespace() => {
                loop {
                    let n = self.txt.next();
                    self.pos.advance(&n);
                    if !matches!(n, Some(c) if c.is_whitespace()) {
                        return (true, n);
                    }
                }
            }
            n => (false, n)
        }
    }

    fn next(&mut self) -> Option<char> {
        let n = self.txt.next();
        self.pos.advance(&n);
        n
    }

    fn try_lex_char_literal(&mut self) -> Result<char, String> {
        let c = match (self.next(), self.next()) {
            (None, _) => return Err(String::from('\'')),
            (Some(c), None) => return Err(format!("'{}", c)),
            (Some('\\'), Some(c@'\'')) |
            (Some('\\'), Some(c@'\"')) |
            (Some('\\'), Some(c@'\\')) => c,
            (Some('\\'), Some('n')) => '\n',
            (Some('\\'), Some('r')) => '\r',
            (Some('\\'), Some('t')) => '\t',
            (Some('\\'), Some('0')) => '\0',
            (Some(c), Some('\'')) => return Ok(c),
            (Some(c), Some(d)) => return Err(format!("'{}{}", c, d)),
        };
        match self.next() {
            None => return Err(format!("'{}", c)),
            Some('\'') => Ok(c),
            Some(x) => return Err(match c {
                '\"' |
                '\\' => format!("'\\{}{}", c, x),
                '\n' => format!("'\\n{}", x),
                '\r' => format!("'\\r{}", x),
                '\t' => format!("'\\t{}", x),
                '\0' => format!("'\\0{}", x),
                c => panic!("incomplete matching {}", c)
            })
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
        let (pos, mut body) = last_sexp.ok_or(LexErrorBody::TooManyClosing)?;

        if self.curr_word.len() > 0 {
            body.push(self.dump_curr());
        }

        self.push_token(Token { pos, body: TokenBody::SExp(body) });
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
        let body = match Keyword::from(self.curr_word.as_str()) {
            Some(kw) => TokenBody::Keyword(kw),
            None => TokenBody::Word(self.curr_word.clone()),
        };
        self.curr_word.clear();
        Token { pos: self.curr_word_start.clone(), body }
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
