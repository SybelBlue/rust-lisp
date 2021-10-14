use std::{iter::Peekable, str::Chars};

use crate::{result::FilePos, token::Token, value::Ident};

#[derive(Debug, Clone, PartialEq)]
pub enum LexError {
    Eof(String),
    BadChar(String, FilePos, char),
    Missing(String, FilePos),
    BadQuote(String, FilePos),
    DupArg {
        name: String,
        old: FilePos,
        new: FilePos,
    },
}

pub type LexResult<T> = Result<T, LexError>;

use LexError::*;

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Eof(x) => write!(f, "Expected {} before end of file", x),
            BadChar(target, ls, c) => write!(f, "Invalid char '{}' for {} at {}", c, target, ls),
            Missing(x, ps) => write!(f, "Missing {} at {}", x, ps),
            BadQuote(x, ps) => write!(f, "Cannot quote before a {} at {}", x, ps),
            DupArg { name, old, new } => {
                write!(f, "Arg \"{}\" defined at {} and {}", name, old, new)
            }
        }
    }
}

#[derive(Debug)]
pub struct LexStream<'a> {
    file_pos: FilePos,
    iter: &'a mut Peekable<Chars<'a>>,
}

impl<'a> LexStream<'a> {
    pub fn new(iter: &'a mut Peekable<Chars<'a>>) -> Self {
        LexStream {
            file_pos: FilePos::new(),
            iter,
        }
    }

    pub fn has_next(&mut self) -> bool {
        self.peek().is_some()
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    pub fn next(&mut self) -> Option<char> {
        let out = self.iter.next();
        if let Some(c) = &out {
            self.file_pos.advance(c);
        }
        out
    }

    pub fn next_if<F: Fn(char) -> bool>(&mut self, f: F) -> Option<char> {
        if let Some(&x) = self.peek() {
            if f(x) {
                self.next();
                return Some(x);
            }
        }
        None
    }

    pub fn next_if_eq(&mut self, c: char) -> Option<bool> {
        if let Some(x) = self.peek() {
            let eq = *x == c;
            if eq {
                self.next();
            }
            Some(eq)
        } else {
            None
        }
    }
}

fn take_while<F: Fn(char) -> bool>(chars: &mut LexStream<'_>, f: F) -> String {
    let mut s = String::new();
    while let Some(&c) = chars.peek() {
        if f(c) {
            s.push(c);
            chars.next();
        } else {
            break;
        }
    }
    s
}

fn many1<F: Fn(char) -> bool>(
    chars: &mut LexStream<'_>,
    target: &'static str,
    f: F,
) -> LexResult<String> {
    let s = take_while(chars, f);
    if s.len() == 0 {
        let ls = chars.file_pos;
        Err(if let Some(c) = chars.peek() {
            BadChar(String::from(target), ls, *c)
        } else {
            Eof(String::from(target))
        })
    } else {
        Ok(s)
    }
}

fn skip_whitespace(chars: &mut LexStream<'_>) {
    take_while(chars, char::is_whitespace);
    if Some(true) == chars.next_if_eq(';') {
        skip_past_eol(chars);
    }
}

fn skip_past_eol(chars: &mut LexStream<'_>) {
    take_while(chars, |c| !is_newline(c));
    chars.next();
    skip_whitespace(chars);
}

fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

pub fn lex_all(chars: &mut LexStream<'_>) -> Vec<LexResult<Token>> {
    let mut v = Vec::new();
    skip_whitespace(chars);
    let mut last_loc = chars.file_pos;
    while chars.peek().is_some() {
        let e = lex_form(chars).or_else(|_| lex_token(chars));
        if last_loc == chars.file_pos {
            chars.next();
            skip_past_eol(chars);
        }
        last_loc = chars.file_pos;
        v.push(e);
    }
    v
}

pub fn lex_form(chars: &mut LexStream<'_>) -> LexResult<Token> {
    struct TokenBuilder(FilePos, Vec<(FilePos, Vec<Token>)>);

    impl TokenBuilder {
        pub fn new(fp: FilePos) -> Self {
            Self(fp, Vec::new())
        }

        pub fn open(&mut self, fp: FilePos) {
            self.1.push((fp, Vec::new()));
        }

        pub fn close(&mut self) -> Option<Token> {
            let (fp, f) = self.1.pop().expect("closing with no open form");

            let new = Token::Form(fp, f);
            if let Some((_, last)) = self.1.last_mut() {
                last.push(new);
                None
            } else {
                Some(new)
            }
        }

        pub fn push(&mut self, t: Token) {
            self.1
                .last_mut()
                .expect("pushing with no open form")
                .1
                .push(t);
        }
    }

    let eof_msg = Eof(format!("closing ')'"));
    let mut builder = TokenBuilder::new(chars.file_pos);
    loop {
        let start = chars.file_pos;
        // let is_quote = chars.next_if_eq('\'') == Some(true);

        if chars.next_if_eq('(').ok_or_else(|| eof_msg.clone())? {
            skip_whitespace(chars);
            builder.open(start);
        } else if chars.next_if_eq(')').ok_or_else(|| eof_msg.clone())? {
            skip_whitespace(chars);
            // let out = if is_quote { Lit(Value::Quote(v)) } else { Form(v) };
            if let Some(t) = builder.close() {
                return Ok(t);
            }
        } else {
            // Ok(if is_quote { Token::from_value(Value::Quote(vec![t]), start) } else { t })
            builder.push(lex_token(chars)?);
        }
    }
}

fn valid_ident_char(c: char) -> bool {
    !c.is_whitespace() && c != '(' && c != ')' && c != '[' && c != ']' && c != ';' && c != '\''
}

fn lex_identifier(chars: &mut LexStream<'_>) -> LexResult<Ident> {
    let file_pos = chars.file_pos;
    let name = many1(chars, "an identifier", valid_ident_char)?;
    skip_whitespace(chars);
    Ok(Ident::new(name, file_pos))
}

fn lex_token(chars: &mut LexStream<'_>) -> LexResult<Token> {
    match lex_identifier(chars) {
        Ok(ident) => Ok(Token::new(ident)),
        Err(BadChar(_, fp, c)) => Err(BadChar(format!("identifier or literal"), fp, c)),
        Err(BadQuote(_, fp)) => Err(BadQuote(format!("identifier or literal"), fp)),
        Err(Eof(_)) => Err(Eof(format!("identifier or literal"))),
        Err(Missing(s, fp)) => Err(Missing(
            format!("{} looking for identifier or literal", s),
            fp,
        )),
        Err(DupArg { name, old, new }) => Err(DupArg { name, old, new }),
    }
}
