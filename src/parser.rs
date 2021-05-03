use std::{iter::Peekable, str::Chars};

use crate::evaluator::{Expr, FilePos, Ident, Token, Value};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    Eof(String),
    BadChar(String, FilePos, char),
    Missing(String, FilePos),
}

pub type ParseResult<T> = Result<T, ParseError>;

use ParseError::*;

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Eof(x) => write!(f, "Expected {} before end of file", x),
            BadChar(target, ls, c) => write!(f, "Invalid char '{}' for {} at {}", c, target, ls),
            Missing(x, ps) => write!(f, "Missing {} at {}", x, ps),
        }
    }
}

#[derive(Debug)]
pub struct ParseStream<'a> {
    file_pos: FilePos,
    iter: &'a mut Peekable<Chars<'a>>,
}

impl<'a> ParseStream<'a> {
    pub fn new(iter: &'a mut Peekable<Chars<'a>>) -> Self {
        Self { file_pos: FilePos::new(), iter }
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
                return Some(x)
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

    pub fn loc(&self) -> FilePos {
        self.file_pos
    }

    pub fn loc_str(&self) -> String {
        format!("{}", self.file_pos)
    }
}

fn take_while<F : Fn(char) -> bool>(chars: &mut ParseStream<'_>, f: F) -> String {
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

fn many1<F : Fn(char) -> bool>(chars: &mut ParseStream<'_>, target: &'static str, f: F) -> ParseResult<String> {
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

fn skip_whitespace(chars: &mut ParseStream<'_>) {
    take_while(chars, char::is_whitespace);
}

fn till_closing_paren(chars: &mut ParseStream<'_>) -> bool {
    let mut n = 1u8;
    while let Some(c) = chars.next() {
        match c {
            '(' => n += 1,
            ')' => {
                if n == 1 {
                    return true;
                } else {
                    n -= 1;
                }
            },
            _ => {},
        }
    }
    false
}

pub fn parse_all(chars: &mut ParseStream<'_>) -> Vec<ParseResult<Token>> {
    let mut v = Vec::new();
    skip_whitespace(chars);
    let mut last_loc = chars.loc();
    while chars.peek().is_some() {
        let e = parse(chars);
        if last_loc == chars.loc() {
            chars.next();
            take_while(chars, |c| c != '\n' && c != '\r');
            skip_whitespace(chars);
        }
        last_loc = chars.loc();
        v.push(e);
    }
    v
}

pub fn parse(chars: &mut ParseStream<'_>) -> ParseResult<Token> {
    use Expr::*;

    let eof_msg = Eof(format!("closing ')'"));
    let start = chars.file_pos;

    if chars.next_if_eq('(').ok_or_else(|| eof_msg.clone())? {
        skip_whitespace(chars);
    } else {
        let id_or_lit_start = chars.file_pos;
        let e = parse_ident_or_literal(chars)?;
        skip_whitespace(chars);
        return match e {
            Ok(v) => Token::from_value(v, id_or_lit_start),
            Err(n) => Ok(Token::new(Var(n.name), n.file_pos)),
        }
    }

    if chars.next_if_eq(')') == Some(true) {
        skip_whitespace(chars);
        return Token::from_value(Value::Unit, start)
    }

    let after_paren = chars.file_pos;
    match parse_ident_or_literal(chars)? {
        Ok(v) => close_target(chars, Token::new(Lit(v), after_paren), "fn declaration"),
        Err(ident) => {
            if ident.name.as_bytes() == b"fn" {
                let f = parse_fn_decl(chars)?;
                return close_target(chars, f, "fn declaration")
            }
        
            let is_defn = ident.name.as_bytes() == b"defn";
            if is_defn || ident.name.as_bytes() == b"def" {
                let name = parse_identifier(chars)?;
                let fn_start = chars.file_pos;
                let e = if is_defn { 
                    parse_fn_decl(chars)?
                } else { 
                    parse(chars)? 
                };
                return close_target(chars, Token::new(Def(name, Box::new(e)), fn_start), "fn declaration")
            }
        
            let mut v = Vec::new();
            loop {
                if chars.next_if_eq(')').ok_or_else(|| eof_msg.clone())? {
                    skip_whitespace(chars);
                    return Ok(Token::new(Form(ident, v), start))
                }
        
                match parse(chars) {
                    Ok(e) => v.push(e),
                    Err(e) => {
                        return Err(if !till_closing_paren(chars) {
                            eof_msg
                        } else {
                            skip_whitespace(chars);
                            e
                        })
                    },
                }
            }
        }
    }
}

fn close_target(chars: &mut ParseStream<'_>, output: Token, target: &str) -> ParseResult<Token> {
    if chars.next_if_eq(')').ok_or_else(|| Eof(format!("closing ')'")))? {
        skip_whitespace(chars);
        Ok(output)
    } else {
        Err(Missing(format!("end of form after {}", target), chars.file_pos))
    }
}

fn parse_fn_decl(chars: &mut ParseStream<'_>) -> ParseResult<Token> {
    let mark = chars.file_pos;
    if chars.next_if_eq('[') != Some(true) {
        return Err(Missing(format!(" arg list, starting with '['"), chars.file_pos));
    }
    
    skip_whitespace(chars);
    let mut params = Vec::new();
    while let Some(&c) = chars.peek() {
        if c == ']' {
            chars.next();
            skip_whitespace(chars);
            let body = Box::new(parse(chars)?);
            return Token::from_value(Value::Fn(params, body), mark)
        }
        params.push(parse_identifier(chars)?);
    }
    Err(Eof(format!("closing ']'")))
}

fn valid_ident_char(c: char) -> bool {
    !c.is_whitespace() && c != '(' && c != ')' && c != '[' && c != ']'
}

pub(crate) fn parse_identifier(chars: &mut ParseStream<'_>) -> ParseResult<Ident> {
    let file_pos = chars.loc();
    let name = many1(chars, "an identifier", valid_ident_char)?;
    skip_whitespace(chars);
    Ok(Ident::new(name, file_pos))
}

fn parse_ident_or_literal(chars: &mut ParseStream<'_>) -> ParseResult<Result<Value, Ident>> {
    match parse_identifier(chars) {
        Ok(n) => {
            Ok(if let Ok(x) = n.name.parse() {
                Ok(Value::Int(x))
            } else if let Ok(x) = n.name.parse() {
                Ok(Value::Float(x))
            } else {
                Err(n)
            })
        },
        Err(BadChar(_, fp, c)) => 
            Err(BadChar(format!("Error parsing identifier or literal"), fp, c)),
        Err(Eof(_)) => 
            Err(Eof(format!("identifier or literal"))),
        Err(Missing(s, fp)) => 
            Err(Missing(format!("{} looking for identifier or literal", s), fp))
    }
}