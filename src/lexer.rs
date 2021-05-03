use std::{collections::VecDeque, iter::Peekable, str::Chars};

use crate::parser::{Expr, FilePos, Ident, Value};

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

fn many1<F : Fn(char) -> bool>(chars: &mut ParseStream<'_>, target: &'static str, f: F) -> Result<String, String> {
    let s = take_while(chars, f);
    if s.len() == 0 {
        let ls = chars.loc_str();
        Err(if let Some(c) = chars.peek() {
            format!("Invalid char '{}' for {} at {}", c, target, ls)
        } else {
            format!("Expected {} before end of file", target)
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

pub fn parse_all(chars: &mut ParseStream<'_>) -> Vec<Result<Expr, String>> {
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

pub fn parse(chars: &mut ParseStream<'_>) -> Result<Expr, String> {
    use Expr::*;

    let eof_msg = format!("Reached end of file before form was closed");
    
    if chars.next_if_eq('(').ok_or_else(|| eof_msg.clone())? {
        skip_whitespace(chars);
    } else {
        let e = safe_parse_number(chars)?;
        skip_whitespace(chars);
        return Ok(match e {
            Ok(v) => Lit(v),
            Err(n) => Idnt(n),
        })
    }

    if chars.next_if_eq(')') == Some(true) {
        skip_whitespace(chars);
        return Ok(Lit(Value::Unit))
    }

    match safe_parse_number(chars)? {
        Ok(v) => close_target(chars, Lit(v), "fn declaration"),
        Err(ident) => {
            if ident.name.as_bytes() == b"fn" {
                let f = parse_fn_decl(chars)?;
                return close_target(chars, Lit(f), "fn declaration")
            }
        
            let is_defn = ident.name.as_bytes() == b"defn";
            if is_defn || ident.name.as_bytes() == b"def" {
                let name = parse_identifier(chars)?;
                let e = if is_defn { Lit(parse_fn_decl(chars)?) } else { parse(chars)? };
                return close_target(chars, Def(name, Box::new(e)), "fn declaration")
            }
        
            let mut v = Vec::new();
            loop {
                if chars.next_if_eq(')').ok_or_else(|| eof_msg.clone())? {
                    skip_whitespace(chars);
                    return Ok(Form(ident, v))
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

fn close_target(chars: &mut ParseStream<'_>, output: Expr, target: &str) -> Result<Expr, String> {
    if chars.next_if_eq(')').ok_or_else(|| format!("Reached end of file before form was closed"))? {
        skip_whitespace(chars);
        Ok(output)
    } else {
        Err(format!("Expecting end of form after {} at {}", target, chars.loc_str()))
    }
}

fn parse_fn_decl(chars: &mut ParseStream<'_>) -> Result<Value, String> {
    if chars.next_if_eq('[') != Some(true) {
        return Err(format!("Missing required arg list, starting with '[' at {}", chars.loc_str()));
    }
    
    skip_whitespace(chars);
    let mut params = VecDeque::new();
    while let Some(&c) = chars.peek() {
        if c == ']' {
            chars.next();
            skip_whitespace(chars);
            let body = Box::new(parse(chars)?);
            return Ok(Value::Fn(params, body))
        }
        params.push_back(parse_identifier(chars)?.name);
    }
    Err(format!("Reached end of file before arg list was closed"))
}

pub(crate) fn parse_identifier(chars: &mut ParseStream<'_>) -> Result<Ident, String> {
    let file_pos = chars.loc();
    let name = many1(chars, "an identifier", |c: char| !c.is_whitespace() && c != '(' && c != ')' && c != '[' && c != ']')?;
    skip_whitespace(chars);
    Ok(Ident::new(name, file_pos))
}

fn safe_parse_number(chars: &mut ParseStream<'_>) -> Result<Result<Value, Ident>, String> {
    let ls = chars.loc_str();
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
        Err(_) => Err(format!("Error parsing identifier or literal at {}", ls)),
    }
}