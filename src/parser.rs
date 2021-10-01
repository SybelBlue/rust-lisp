use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::evaluator::{expr::Expr, token::Token, result::FilePos, value::{Ident, Value}};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    Eof(String),
    BadChar(String, FilePos, char),
    Missing(String, FilePos),
    BadQuote(String, FilePos),
    DupArg { name: String, old: FilePos, new: FilePos },
}

pub type ParseResult<T> = Result<T, ParseError>;

use ParseError::*;

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Eof(x) => write!(f, "Expected {} before end of file", x),
            BadChar(target, ls, c) => write!(f, "Invalid char '{}' for {} at {}", c, target, ls),
            Missing(x, ps) => write!(f, "Missing {} at {}", x, ps),
            BadQuote(x, ps) => write!(f, "Cannot quote before a {} at {}", x, ps),
            DupArg { name, old, new } => write!(f, "Arg \"{}\" defined at {} and {}", name, old, new),
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
    if Some(true) == chars.next_if_eq(';')  {
        skip_past_eol(chars);
    }
}

fn skip_past_eol(chars: &mut ParseStream<'_>) {
    take_while(chars, |c| !is_newline(c));
    chars.next();
    skip_whitespace(chars);
}

fn till_closing_paren(chars: &mut ParseStream<'_>) -> bool {
    let mut n = 1u8;
    while let Some(c) = chars.next() {
        match c {
            '(' => n += 1,
            ')' => {
                if n == 1 {
                    skip_whitespace(chars);
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

fn is_newline(c: char) -> bool {
    c == '\n' || c == '\r'
}

pub fn parse_all(chars: &mut ParseStream<'_>) -> Vec<ParseResult<Token>> {
    let mut v = Vec::new();
    skip_whitespace(chars);
    let mut last_loc = chars.file_pos;
    while chars.peek().is_some() {
        let e = parse(chars);
        if last_loc == chars.file_pos {
            chars.next();
            skip_past_eol(chars);
        }
        last_loc = chars.file_pos;
        v.push(e);
    }
    v
}

pub fn parse(chars: &mut ParseStream<'_>) -> ParseResult<Token> {
    use Expr::*;

    let eof_msg = Eof(format!("closing ')'"));
    let start = chars.file_pos;

    let is_quote = chars.next_if_eq('\'') == Some(true);

    if chars.next_if_eq('(').ok_or_else(|| eof_msg.clone())? {
        skip_whitespace(chars);
        let mut v = Vec::new();
        loop {
            match chars.next_if_eq(')') {
                None => return Err(eof_msg),
                Some(true) => { 
                    skip_whitespace(chars); 
                    // let out = if is_quote { Lit(Value::Quote(v)) } else { Form(v) };
                    unimplemented!();
                    // return Ok(Token::new(out, start));
                },
                Some(false) => {
                    match parse(chars) {
                        Ok(e) => {
                            if v.is_empty() {
                                if let Var(s) = &e.expr {
                                    let sf = parse_special_form(chars, s, start, is_quote)?;
                                    if let Some(out) = sf {
                                        return Ok(out)
                                    }
                                }
                            }
                            v.push(e);
                        },
                        Err(e) => {
                            return Err(if !till_closing_paren(chars) {
                                eof_msg
                            } else {
                                e
                            })
                        },
                    }
                },
            }
        }
    } else {
        let t = match parse_ident_or_literal(chars)? {
            Ok(v) => Token::new(Lit(v), start),
            Err(ident) => Token::from_ident(ident),
        };
        unimplemented!();
        // Ok(if is_quote { Token::from_value(Value::Quote(vec![t]), start) } else { t })
    }
}

fn parse_special_form(chars: &mut ParseStream<'_>, s: &String, start: FilePos, is_quote: bool) -> ParseResult<Option<Token>> {
    if s.as_bytes() == b"fn" {
        if is_quote { 
            till_closing_paren(chars);
            return Err(BadQuote(format!("fn declaration"), start)) 
        }
        let f = parse_fn_decl(chars, false)?;
        let out = close_target(chars, f, "fn declaration")?;
        return Ok(Some(out));
    }

    let is_def = s.as_bytes() == b"def";
    let is_macro = s.as_bytes() == b"macro";
    if is_def || is_macro || s.as_bytes() == b"defn" {
        if is_quote { 
            till_closing_paren(chars);
            return Err(BadQuote(format!("def"), start)) 
        }
        let name = parse_identifier(chars)?;
        let fn_start = chars.file_pos;
        let e = if is_def { 
            parse(chars)
        } else { 
            parse_fn_decl(chars, is_macro)
        }?;
        unimplemented!()
        // let out = close_target(chars, Token::new(Expr::Def(name, Box::new(e)), fn_start), "def")?;
        // return Ok(Some(out));
    }

    if s.as_bytes() == b"import" {
        if is_quote {
            till_closing_paren(chars);
            return Err(BadQuote(format!("import"), start))
        }

        let name = parse_identifier(chars)?;
        let alias = parse_identifier(chars).ok();
        unimplemented!()
        // let output = Token::new(Expr::Import(name, alias), start);
        // let out = close_target(chars, output, "import")?;
        // return Ok(Some(out));
    }

    Ok(None)
}

fn close_target(chars: &mut ParseStream<'_>, output: Token, target: &str) -> ParseResult<Token> {
    if chars.next_if_eq(')').ok_or_else(|| Eof(format!("closing ')'")))? {
        skip_whitespace(chars);
        Ok(output)
    } else {
        Err(Missing(format!("end of form after {}", target), chars.file_pos))
    }
}

fn parse_fn_decl(chars: &mut ParseStream<'_>, is_macro: bool) -> ParseResult<Token> {
    let mark = chars.file_pos;
    if chars.next_if_eq('[') != Some(true) {
        return Err(Missing(format!(" arg list, starting with '['"), chars.file_pos));
    }
    
    skip_whitespace(chars);
    let mut params: Vec<Ident> = Vec::new();
    let mut quoted = Vec::new();
    let mut p_map = HashMap::with_capacity(6);
    loop {
        match chars.next_if_eq(']') {
            None => return Err(Eof(format!("closing ']'"))),
            Some(true) => {
                skip_whitespace(chars);
                let body = Box::new(parse(chars)?);
                let op_rest = 
                    if let (Some(p), Some(q)) = (params.last(), quoted.last()) {
                        if let Some(new) = p.name.strip_prefix("...") {
                            if *q {
                                return Err(BadQuote(format!("macro arg list rest"), p.file_pos))
                            }
                            let new_pos = &mut p.file_pos.clone();
                            new_pos.col += 3;
                            Ok(Some(Ident::new(String::from(new), *new_pos)))
                        } else { 
                            Ok(None)
                        }   
                    } else { Ok(None) }?;
                
                if op_rest.is_some() {
                    params.pop();
                }

                return Ok(Token::from_value(
                    if is_macro {
                        unimplemented!()
                        // Value::Macro(quoted.into_iter().zip(params.into_iter()).collect(), op_rest, body)
                    } else {
                        Value::Fn(params, op_rest, body)
                    }, 
                    mark))
            },
            Some(false) => {
                let is_macro_quoted = is_macro && chars.next_if_eq('\'') == Some(true);
                let ident = parse_identifier(chars)?;
                params.push(ident.clone());
                quoted.push(is_macro_quoted);
                if let Some(old) = p_map.insert(ident.name.clone(), ident.file_pos) {
                    till_closing_paren(chars);
                    return Err(DupArg { name: ident.name, old, new: ident.file_pos })
                }
            }
        }
    }
}

fn valid_ident_char(c: char) -> bool {
    !c.is_whitespace() 
        && c != '(' && c != ')' 
        && c != '[' && c != ']' 
        && c != ';' 
        && c != '\''
}

fn parse_identifier(chars: &mut ParseStream<'_>) -> ParseResult<Ident> {
    let file_pos = chars.file_pos;
    let name = many1(chars, "an identifier", valid_ident_char)?;
    skip_whitespace(chars);
    Ok(Ident::new(name, file_pos))
}

fn parse_ident_or_literal(chars: &mut ParseStream<'_>) -> ParseResult<Result<Value, Ident>> {
    match parse_identifier(chars) {
        Ok(n) => {
            Ok(if let Ok(x) = n.name.parse() {
                Ok(Value::Int(x))
            // } else if let Ok(x) = n.name.parse() {
            //     Ok(Value::Float(x))
            } else {
                Err(n)
            })
        },
        Err(BadChar(_, fp, c)) => 
            Err(BadChar(format!("identifier or literal"), fp, c)),
        Err(BadQuote(_, fp)) => 
            Err(BadQuote(format!("identifier or literal"), fp)),
        Err(Eof(_)) => 
            Err(Eof(format!("identifier or literal"))),
        Err(Missing(s, fp)) => 
            Err(Missing(format!("{} looking for identifier or literal", s), fp)),
        Err(DupArg { name, old, new }) =>
            Err(DupArg { name, old, new }),
    }
}