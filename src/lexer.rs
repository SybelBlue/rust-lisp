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

fn followedBy<F : Fn(char) -> bool, G : Fn(char) -> bool>(chars: &mut Peekable<Chars<'_>>, target: &'static str, start: F, rest: G) -> Result<String, String> {
    let &c = check_eof(chars.peek(), target)?;
    if !start(c) {
        return Err(format!("Invalid start char {} for {}", c, target))
    }
    let mut s = String::new();
    s.push(c);
    chars.next();
    s.push_str(take_while(chars, rest).as_str());
    Ok(s)
}

fn skip_whitespace(chars: &mut Peekable<Chars<'_>>) {
    take_while(chars, char::is_whitespace);
}

fn check_eof<T>(item: Option<T>, target: &'static str) -> Result<T, String> {
    item.ok_or_else(|| format!("Hit end of file expecting {}", target))
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
    skip_whitespace(chars);
    Ok(exp)
}