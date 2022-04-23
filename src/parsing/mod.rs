pub mod lex;
pub mod sources;

use std::collections::HashSet;

use crate::{exprs::{Expr, values::Value, SToken, SExp}, errors::{ParseErrorBody::*, ParseResult, Loc}};

use self::{lex::Token, sources::FilePos};


pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Expr<'a>>> {
    let mut ts = ts.into_iter();
    let mut exprs = Vec::new();

    if let Some(t) = ts.next() {
        match parse_first(t)? {
            Err(lam_fp) => {
                let t = ts.next().ok_or(Loc::new(lam_fp.clone(), MissingLambdaParams))?;
                let p = parse_rest(check_params(t, lam_fp.clone())?)?;
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
        Token::SExp(Loc { pos: locable, body }) => {
            let body = parse_tokens(body.0)
                .map_err(|e| Loc::new(locable.clone(), InSExp(Box::new(e))))?;
            Ok(Expr::SExp(SToken::new(locable, SExp(body))))
        },
    })
}

fn check_params<'a>(t: Token<'a>, start: FilePos<'a>) -> ParseResult<'a, Token<'a>> {
    let mut used = HashSet::new();
    let mut to_check = vec![&t];
    while let Some(next) = to_check.pop() {
        match next {
            Token::LamSlash(fp) => return Err(Loc::new(fp.clone(), MisplacedLambda)),
            Token::Word(w) =>
                if !used.insert(w.clone()) {
                    return Err(Loc::new(start.clone(), DuplicateLambdaArg(w.clone())))
                },
            Token::SExp(sbody) => to_check.extend(&sbody.body.0),
        }
    }
    Ok(t)
}