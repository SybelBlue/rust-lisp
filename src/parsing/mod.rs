pub mod lex;
pub mod sources;

use std::collections::HashSet;

use crate::{exprs::{Expr, values::Value, SToken, SExp}, errors::{ParseErrorBody::*, ParseResult, Loc}};

use self::{lex::Token, sources::FilePos};


pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Expr<'a>>> {
    if ts.is_empty() { return Ok(Vec::new()) }

    let mut ts = ts.into_iter();
    
    let fst_tkn = ts.next().unwrap();

    let snd = if let Some(t) = ts.next() {
        parse_catch_arrow(t)?
    } else {
        return Ok(vec![parse(fst_tkn)?]);
    };

    match snd {
        Err((false, _pos)) => {
            todo!("make binds")
        }
        Err((true, pos)) => {
            check_params(&fst_tkn)?;
            let params = parse(fst_tkn)?;
            if let Some(t) = ts.next() {
                if ts.next().is_none() {
                    let body = parse_simple(t)?;
                    Ok(vec![Expr::Val(Loc::new(pos, Value::Lam(Box::new(params), Box::new(body))))])
                } else {
                    Err(Loc::new(pos, ExtraLambdaBody))
                }
            } else {
                Err(Loc::new(pos, MissingLambdaBody))
            }
        }
        Ok(snd) => {
            let mut out = vec![parse(fst_tkn)?, snd];
            for t in ts {
                out.push(parse_simple(t)?);
            }
            Ok(out)
        }
    }
}

fn parse_simple<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    parse_catch_arrow(t)?.map_err(|(_, fp)| Loc::new(fp, MisplacedArrow))
}

fn parse<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    match parse_catch_arrow(t)? {
        Ok(e) => Ok(e),
        Err((true, pos)) => Ok(Expr::Val(Loc::new(pos, Value::Sym(String::from("->"))))),
        Err((false, pos)) => Err(Loc::new(pos, MisplacedArrow)),
    }
}

/// If successful, returns an expr or the direction and location of an arrow
fn parse_catch_arrow<'a>(t: Token<'a>) -> ParseResult<'a, Result<Expr<'a>, (bool, FilePos<'a>)>> {
    Ok(match t {
        Token::Word(w, pos) =>
            Ok(if let Ok(n) = w.parse::<usize>() {
                Expr::Val(Loc::new(pos, Value::Nat(n)))
            } else {
                Expr::Val(Loc::new(pos, Value::Sym(w)))
            }),
        Token::SExp(Loc { pos: locable, body }) => {
            let body = parse_tokens(body.0)
                .map_err(|e| Loc::new(locable.clone(), InSExp(Box::new(e))))?;
            Ok(Expr::SExp(SToken::new(locable, SExp(body))))
        },
        Token::Arrow(forward, pos) => Err((forward, pos)),
    })
}

fn check_params<'a>(t: &Token<'a>) -> ParseResult<'a, ()> {
    let mut used = HashSet::new();
    let mut to_check = vec![t];
    while let Some(next) = to_check.pop() {
        match next {
            Token::Arrow(_, fp) => return Err(Loc::new(fp.clone(), MisplacedArrow)),
            Token::Word(w, pos) =>
                if !used.insert(w.clone()) {
                    return Err(Loc::new(pos.clone(), DuplicateLambdaArg(w.clone())))
                },
            Token::SExp(sbody) => to_check.extend(sbody.body.0.iter().rev()),
        }
    }
    Ok(())
}