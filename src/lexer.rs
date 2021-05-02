use std::{iter::Peekable, str::Chars};

use crate::parser::Expr;

fn take_while<F : Fn(char) -> bool>(chars: &mut Peekable<Chars<'_>>, f: F) -> String {
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

fn optional<F : Fn(char) -> bool>(chars: &mut Peekable<Chars<'_>>, f: F) -> Option<char> {
    if let Some(&c) = chars.peek() {
        if f(c) {
            chars.next();
            return Some(c);
        }
    }
    None
}

fn many1<F : Fn(char) -> bool>(chars: &mut Peekable<Chars<'_>>, target: &'static str, f: F) -> Result<String, String> {
    let s = take_while(chars, f);
    if s.len() == 0 {
        Err(if let Some(c) = chars.peek() {
            format!("Invalid char {} for {}", c, target)
        } else {
            format!("Expected {} before end of file", target)
        })
    } else {
        Ok(s)
    }
}

fn skip_whitespace(chars: &mut Peekable<Chars<'_>>) {
    take_while(chars, char::is_whitespace);
}

pub fn parse(chars: &mut Peekable<Chars<'_>>) -> Result<Expr, String> {
    use Expr::*;
    
    if let Ok(e) = parse_number(chars) { 
        skip_whitespace(chars);
        return Ok(e); 
    }
    
    match chars.next() {
        None => Err(format!("Expected a form, got end of file")),
        Some('(') => Ok(()),
        Some(c) => Err(format!("Expected number or form, got {}", c)),
    }?;

    skip_whitespace(chars);
    if chars.peek() == Some(&')') {
        chars.next();
        skip_whitespace(chars);
        return Ok(Unit);
    }

    let ident = Box::new(Ident(parse_identifier(chars)?));
    let mut v = Vec::new();
    while let Some(&c) = chars.peek() {
        if c == ')' {
            chars.next();
            skip_whitespace(chars);
            return Ok(Form(ident, v))
        }

        v.push(parse(chars)?);
    }
    
    Err(format!("Expected ')' to close the form, got end of file"))
}

pub(crate) fn parse_identifier(chars: &mut Peekable<Chars<'_>>) -> Result<String, String> {
    let out = many1(chars, "an identifier", |c: char| !c.is_whitespace() && c != '(' && c != ')');
    skip_whitespace(chars);
    out
}

pub(crate) fn parse_number(chars: &mut Peekable<Chars<'_>>) -> Result<Expr, String> {
    let mut num = String::new();
    
    if optional(chars, |c| c == '-').is_some() {
        num.push('-');
    }
    
    num.push_str(many1(chars, "an integer", char::is_numeric)?.as_str());
    
    let exp = if optional(chars, |c| c == '.').is_some() {
        num.push('.');
        num.push_str(many1(chars, "a decimal part", char::is_numeric)?.as_str());
        num.parse().map_err(|_| format!("bad float literal {}", num)).map(Expr::Float)
    } else {
        num.parse().map_err(|_| format!("bad int literal {}", num)).map(Expr::Int)
    }?;
    if !matches!(chars.peek(), Some(&c) if c.is_whitespace() || c == ')') {
        Err(format!("Missing required whitespace or ) at the end of a number literal"))
    } else {
        skip_whitespace(chars);
        Ok(exp)
    }
}