pub mod lex;
pub mod sources;

use std::collections::HashSet;

use crate::{exprs::{Expr, values::Value, SToken, SExp}, errors::{ParseErrorBody::*, ParseResult, Loc}};

use self::{lex::Token, sources::FilePos};


pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Expr<'a>>> {
    let mut ts = ts.into_iter();
    match (ts.next(), ts.next()) {
        // is unit
        (None, _) => Ok(vec![]),
        // is singleton
        (Some(t), None) => Ok(vec![parse_simple(t)?]),
        // has at least two 
        (Some(fst), Some(snd)) => {
            let param_check = check_params(&fst);
            match (parse_one(fst)?, parse_one(snd)?) {
                // both are arrows!
                (Err((_, pos)), Err(_)) => Err(Loc::new(pos, MisplacedArrow)),
                // fst is backarrow!
                (Err((false, pos)), _) => Err(Loc::new(pos, MisplacedArrow)),
                // fst is arrow, snd not arrow...
                (Err((true, pos)), Ok(e)) => {
                    let mut out = vec![Expr::Val(Loc::new(pos, Value::Sym(String::from("->")))), e];
                    for t in ts {
                        out.push(parse_simple(t)?);
                    }
                    Ok(out)
                }
                // fst not arrow, snd is backarrow...
                (Ok(_e), Err((false, _pos))) => {
                    todo!("make binds")
                }
                // fst not arrow, snd is forward arrow..
                (Ok(params), Err((true, pos))) => {
                    param_check?;
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
                // neither are arrows..
                (Ok(fst), Ok(snd)) => {
                    let mut out = vec![fst, snd];
                    for t in ts {
                        out.push(parse_simple(t)?);
                    }
                    Ok(out)
                }
            }
        }
    }
}

fn parse_simple<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    parse_one(t)?.map_err(|(_, fp)| Loc::new(fp, MisplacedArrow))
}

/// If successful, returns an expr or the direction and location of an arrow
fn parse_one<'a>(t: Token<'a>) -> ParseResult<'a, Result<Expr<'a>, (bool, FilePos<'a>)>> {
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
            Token::SExp(sbody) => to_check.extend(&sbody.body.0),
        }
    }
    Ok(())
}