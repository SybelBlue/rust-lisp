use std::{collections::VecDeque, iter::Peekable, str::Chars};

use crate::parser::{Expr, Value};

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
        return Ok(Lit(e)); 
    }

    let eof_err = Err(format!("Reached end of file before form was closed"));
    
    match chars.peek() {
        None => return eof_err,
        Some('(') => { chars.next(); skip_whitespace(chars) },
        Some(_) => return parse_identifier(chars).map(Ident),
    }

    if chars.peek() == Some(&')') {
        chars.next();
        skip_whitespace(chars);
        return Ok(Lit(Value::Unit));
    }

    let ident = parse_identifier(chars)?;

    if ident.as_bytes() == b"fn" {
        let f = parse_fn_decl(chars)?;
        return match chars.next() {
            None => eof_err,
            Some(')') => { skip_whitespace(chars); Ok(Lit(f)) },
            Some(c) => Err(format!("Expecting end of form after fn declaration, got {} instead", c)),
        }
    }

    let is_defn = ident.as_bytes() == b"defn";
    if is_defn || ident.as_bytes() == b"def" {
        let name = parse_identifier(chars)?;
        let e = if is_defn { Lit(parse_fn_decl(chars)?) } else { parse(chars)? };
        return match chars.next() {
            None => eof_err,
            Some(')') => {skip_whitespace(chars); Ok(Def(name, Box::new(e))) },
            Some(c) => Err(format!("Expecting end of form after defn, got {} instead", c)),
        }
    }

    let mut v = Vec::new();
    while let Some(&c) = chars.peek() {
        if c == ')' {
            chars.next();
            skip_whitespace(chars);
            if v.len() == 0 {
                if let Ok(e) = parse_number(&mut ident.chars().peekable()) {
                    return Ok(Lit(e));
                } else {
                    return Ok(Ident(ident))
                }
            }
            return Ok(Form(ident, v))
        }

        match parse(chars) {
            Ok(e) => v.push(e),
            Err(e) => {
                return if !till_closing_paren(chars) {
                    eof_err
                } else {
                    skip_whitespace(chars);
                    Err(e)
                }
            },
        }
    }
    
    eof_err
}

fn parse_fn_decl(chars: &mut Peekable<Chars<'_>>) -> Result<Value, String> {
    if chars.peek() != Some(&'[') {
        return Err(format!("Missing required arg list, starting with '['"));
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

pub(crate) fn parse_identifier(chars: &mut Peekable<Chars<'_>>) -> Result<String, String> {
    let out = many1(chars, "an identifier", |c: char| !c.is_whitespace() && c != '(' && c != ')' && c != '[' && c != ']')?;
    skip_whitespace(chars);
    Ok(out)
}

pub(crate) fn parse_number(chars: &mut Peekable<Chars<'_>>) -> Result<Value, String> {
    let mut num = String::new();
    
    if optional(chars, |c| c == '-').is_some() {
        num.push('-');
    }
    
    num.push_str(many1(chars, "an integer", char::is_numeric)?.as_str());
    
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