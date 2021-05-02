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
            format!("Invalid char '{}' for {}", c, target)
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

fn till_closing_paren(chars: &mut Peekable<Chars<'_>>) -> bool {
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

pub fn parse_all(chars: &mut Peekable<Chars<'_>>) -> Vec<Result<Expr, String>> {
    let mut v = Vec::new();
    skip_whitespace(chars);
    while chars.peek().is_some() {
        let e = parse(chars);
        v.push(e);
    }
    v
}

pub fn parse(chars: &mut Peekable<Chars<'_>>) -> Result<Expr, String> {
    use Expr::*;
    
    if let Ok(e) = parse_number(chars) { 
        skip_whitespace(chars);
        return Ok(e); 
    }
    
    match chars.peek() {
        None => return Err(format!("Expected a form, got end of file")),
        Some('(') => { chars.next(); skip_whitespace(chars) },
        Some(_) => return parse_identifier(chars).map(Ident),
    }

    if chars.peek() == Some(&')') {
        chars.next();
        skip_whitespace(chars);
        return Ok(Unit);
    }

    let ident = parse_identifier(chars)?;
    let mut v = Vec::new();
    while let Some(&c) = chars.peek() {
        if c == ')' {
            chars.next();
            skip_whitespace(chars);
            if v.len() == 0 {
                if let Ok(e) = parse_number(&mut ident.chars().peekable()) {
                    return Ok(e);
                } else {
                    return Ok(Ident(ident))
                }
            }
            return Ok(Form(Box::new(Ident(ident)), v))
        }

        match parse(chars) {
            Ok(e) => v.push(e),
            Err(e) => {
                return if !till_closing_paren(chars) {
                    Err(format!("Reached end of file before form was closed"))
                } else {
                    skip_whitespace(chars);
                    Err(e)
                }
            },
        }
    }
    
    Err(format!("Reached end of file before form was closed"))
}

pub(crate) fn parse_identifier(chars: &mut Peekable<Chars<'_>>) -> Result<String, String> {
    let out = many1(chars, "an identifier", |c: char| !c.is_whitespace() && c != '(' && c != ')')?;
    skip_whitespace(chars);
    Ok(out)
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
    skip_whitespace(chars);
    Ok(exp)
}