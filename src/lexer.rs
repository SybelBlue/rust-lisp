use std::{collections::VecDeque, iter::Peekable, str::Chars};

use crate::parser::{Expr, Value};

#[derive(Debug)]
pub struct ParseStream<'a> {
    col: usize,
    line: usize,
    iter: &'a mut Peekable<Chars<'a>>,
}

impl<'a> ParseStream<'a> {
    pub fn new(iter: &'a mut Peekable<Chars<'a>>) -> Self {
        Self { col: 1, line: 1, iter }
    }

    pub fn has_next(&mut self) -> bool {
        self.peek().is_some()
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    pub fn next(&mut self) -> Option<char> {
        let out = self.iter.next();
        if out == Some('\n') || out == Some('\r') {
            self.col = 1;
            self.line += 1;
        } else {
            self.col += 1;
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

    pub fn loc(&self) -> (usize, usize) {
        (self.line, self.col)
    }

    pub fn loc_str(&self) -> String {
        format!("{}:{}", self.line, self.col)
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

fn optional<F : Fn(char) -> bool>(chars: &mut ParseStream<'_>, f: F) -> Option<char> {
    if let Some(&c) = chars.peek() {
        if f(c) {
            chars.next();
            return Some(c);
        }
    }
    None
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
        // if matches!(e, Err(_)) { panic!(format!("{:?}", e)) } else { println!("parse all {:?}", e) }
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
    
    if let Ok(e) = safe_parse_number(chars) { 
        skip_whitespace(chars);
        return Ok(match e {
            Ok(v) => Lit(v),
            Err(n) => Ident(n),
        })
    }

    let eof_msg = format!("Reached end of file before form was closed");
    
    if chars.next_if_eq('(').ok_or_else(|| eof_msg.clone())? {
        skip_whitespace(chars)
    } else {
        return parse_identifier(chars).map(Ident)
    }

    if chars.next_if_eq(')') == Some(true) {
        skip_whitespace(chars);
        return Ok(Lit(Value::Unit))
    }

    let ident = parse_identifier(chars)?;

    if ident.as_bytes() == b"fn" {
        let f = parse_fn_decl(chars)?;
        let ls = chars.loc_str();
        return if chars.next_if_eq(')').ok_or_else(|| eof_msg.clone())? {
            skip_whitespace(chars);
            Ok(Lit(f)) 
        } else {
            Err(format!("Expecting end of form after fn declaration at {}", ls))
        }
    }

    let is_defn = ident.as_bytes() == b"defn";
    if is_defn || ident.as_bytes() == b"def" {
        let name = parse_identifier(chars)?;
        let e = if is_defn { Lit(parse_fn_decl(chars)?) } else { parse(chars)? };
        let ls = chars.loc_str();
        return if chars.next_if_eq(')').ok_or_else(|| eof_msg.clone())? {
            skip_whitespace(chars);
            Ok(Def(name, Box::new(e))) 
        } else {
            Err(format!("Expecting end of form after defn declaration at {}", ls))
        }
    }

    let mut v = Vec::new();
    loop {
        if chars.next_if_eq(')').ok_or_else(|| eof_msg.clone())? {
            skip_whitespace(chars);
            if v.len() == 0 {
                if let Ok(e) = greedy_parse_number(&mut ParseStream::new(&mut ident.chars().peekable())) {
                    return Ok(Lit(e));
                } else {
                    return Ok(Ident(ident))
                }
            }
            return Ok(Form(ident, v))
        }

        println!("arg parse at {}: {:?}", chars.loc_str(), chars.peek());
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

fn parse_fn_decl(chars: &mut ParseStream<'_>) -> Result<Value, String> {
    if chars.peek() != Some(&'[') {
        return Err(format!("Missing required arg list, starting with '[' at {}", chars.loc_str()));
    }
    chars.next();
    skip_whitespace(chars);
    let mut params = VecDeque::new();
    while let Some(&c) = chars.peek() {
        if c == ']' {
            chars.next();
            skip_whitespace(chars);
            let body = Box::new(parse(chars)?);
            return Ok(Value::Fn(params, body))
        }
        params.push_back(parse_identifier(chars)?);
    }
    Err(format!("Reached end of file before arg list was closed"))
}

pub(crate) fn parse_identifier(chars: &mut ParseStream<'_>) -> Result<String, String> {
    println!("start ident at {} w/{:?}", chars.loc_str(), chars.peek());
    let out = many1(chars, "an identifier", |c: char| !c.is_whitespace() && c != '(' && c != ')' && c != '[' && c != ']')?;
    println!("finished ident {} at {}", out, chars.loc_str());
    skip_whitespace(chars);
    Ok(out)
}

pub(crate) fn safe_parse_number(chars: &mut ParseStream<'_>) -> Result<Result<Value, String>, String> {
    let ls = chars.loc_str();
    match parse_identifier(chars) {
        Ok(n) => {
            let n_cs = &mut n.chars().peekable();
            let n_ps = &mut ParseStream::new(n_cs);
            return if let Ok(v) = greedy_parse_number(n_ps) {
                Ok(Ok(v))
            } else {
                Ok(Err(n))
            }
        },
        Err(_) => Err(format!("Error parsing number at {}", ls)),
    }
}

pub(crate) fn greedy_parse_number(chars: &mut ParseStream<'_>) -> Result<Value, String> {
    let negate = chars.next_if_eq('-') == Some(true); // consumes leading '-' even when fails!
    Ok(match parse_pos_number(chars)? {
        Value::Int(x) if negate => Value::Int(-x),
        Value::Float(x) if negate => Value::Float(-x),
        v => v,
    })
}

fn parse_pos_number(chars: &mut ParseStream<'_>) -> Result<Value, String> {
    let mut num = many1(chars, "an integer", char::is_numeric)?;
    
    let exp = if optional(chars, |c| c == '.').is_some() {
        num.push('.');
        num.push_str(many1(chars, "a decimal part", char::is_numeric)?.as_str());
        num.parse().map_err(|_| format!("bad float literal {}", num)).map(Value::Float)
    } else {
        num.parse().map_err(|_| format!("bad int literal {}", num)).map(Value::Int)
    }?;
    skip_whitespace(chars);
    Ok(exp)
}