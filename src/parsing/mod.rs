pub mod lex;

use std::collections::HashSet;

use crate::{exprs::{Expr, values::Value, SBody}, errors::{ParseErrorBody::*, ParseResult, Loc}};

use self::lex::Token;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePos<'a> {
    pub name: Option<&'a String>,
    pub row: usize,
    pub col: usize,
}

impl<'a> FilePos<'a> {
    pub fn new(name: Option<&'a String>) -> Self {
        Self { name, row: 1, col: 1 }
    }

    pub fn advance(&mut self, och: &Option<char>) {
        match och {
            None => {},
            Some('\n') => {
                self.row += 1;
                self.col = 1;
            },
            _ => self.col += 1,
        }
    }

    pub fn col_arrow(&self) -> String {
        let mut out = String::new();
        out.extend((0..(self.col - 2)).map(|_| ' '));
        out.push('^');
        out
    }
}

impl<'a> std::fmt::Display for FilePos<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", 
            self.name.unwrap_or(&String::from("anon")), 
            self.row,
            self.col)
    }
}

pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Expr<'a>>> {
    let mut ts = ts.into_iter();
    let mut exprs = Vec::new();

    if let Some(t) = ts.next() {
        match parse_first(t)? {
            Err(lam_fp) => {
                let t = ts.next().ok_or(Loc::new(lam_fp.clone(), MissingLambdaParams))?;
                let p = parse_rest(check_params(t)?)?;
                let t = ts.next().ok_or(Loc::new(lam_fp.clone(), MissingLambdaBody))?;
                let b = parse_rest(t)?;
                return if ts.next().is_some() {
                    Err(Loc::new(lam_fp, ExtraLambdaBody))
                } else {
                    Ok(vec![Expr::Val(Value::Lam(Box::new(p), Box::new(b)))])
                };
            },
            Ok(e) => exprs.push(e)
        }
    } else {
        return Ok(exprs);
    }

    for t in ts {
        exprs.push(parse_rest(t)?);
    }

    Ok(exprs)
}

fn parse_rest<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    parse_first(t)?.map_err(|fp| Loc::new(fp, MisplacedLambda))
}

fn parse_first<'a>(t: Token<'a>) -> ParseResult<'a, Result<Expr<'a>, FilePos<'a>>> {
    Ok(match t {
        Token::LamSlash(fp) => Err(fp),
        Token::Word(w) =>
            Ok(if let Ok(n) = w.parse::<usize>() {
                Expr::Val(Value::Nat(n))
            } else {
                Expr::Val(Value::Sym(w))
            }),
        Token::SExp(SBody { start, body }) => {
            let body = parse_tokens(body)
                .map_err(|e| Loc::new(start.clone(), InSExp(Box::new(e))))?;
            Ok(Expr::SExp(SBody { start, body }))
        },
    })
}

fn check_params<'a>(t: Token<'a>) -> ParseResult<'a, Token<'a>> {
    let mut used = HashSet::new();
    let mut to_check = vec![&t];
    while let Some(next) = to_check.pop() {
        match next {
            Token::LamSlash(fp) => return Err(Loc::new(fp.clone(), MisplacedLambda)),
            Token::Word(w) =>
                if !used.insert(w.clone()) {
                    return Err(Loc::new(fp, DuplicateLambdaArg(w.clone())))
                },
            Token::SExp(sbody) => to_check.extend(&sbody.body),
        }
    }
    Ok(t)
}