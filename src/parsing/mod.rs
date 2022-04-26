pub mod lex;
pub mod sources;

use std::collections::HashSet;

use crate::{exprs::{Expr, values::{Value, VToken}, SToken, SExp}, errors::{ParseResult, ParseError}};

use crate::errors::ParseErrorBody::*;
use crate::parsing::lex::Keyword::*;

use self::{lex::{Token, Keyword}, sources::FilePos};


pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Expr<'a>>> {
    if ts.is_empty() { return Ok(Vec::new()) }

    let mut ts = ts.into_iter();
    
    let fst_tkn = ts.next().unwrap();

    let snd = if let Some(t) = ts.next() {
        parse_catch_keyword(t)?
    } else {
        return Ok(vec![parse_first(fst_tkn)?]);
    };

    match snd {
        Err((Backarrow, pos)) => {
            Err(ParseError::new(pos, NotYetImplemented("Backarrow bind")))
        }
        Err((Arrow, pos)) => {
            check_params(&fst_tkn)?;
            let params = parse_first(fst_tkn)?;
            if let Some(t) = ts.next() {
                if ts.next().is_none() {
                    let body = parse_simple(t)?;
                    Ok(vec![Expr::Val(VToken::new(pos, Value::Lam(Box::new(params), Box::new(body))))])
                } else {
                    Err(ParseError::new(pos, ExtraLambdaBody))
                }
            } else {
                Err(ParseError::new(pos, MissingLambdaBody))
            }
        }
        Err((Trait, pos)) => {
            Err(ParseError::new(pos, NotYetImplemented("traits")))
        }
        Err((Data, pos)) => {
            Err(ParseError::new(pos, NotYetImplemented("data")))
        }
        Ok(snd) => {
            let mut out = vec![parse_first(fst_tkn)?, snd];
            for t in ts {
                out.push(parse_simple(t)?);
            }
            Ok(out)
        }
    }
}

fn parse_simple<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    parse_catch_keyword(t)?.map_err(|(kw, fp)| 
        ParseError::new(fp, MisplacedKeyword(kw)))
}

fn parse_first<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    match parse_catch_keyword(t)? {
        Ok(e) => Ok(e),
        Err((Arrow, pos)) => Ok(Expr::Val(VToken::new(pos, Value::Sym(String::from("->"))))),
        Err((Backarrow, pos)) => Err(ParseError::new(pos, MisplacedKeyword(Backarrow))),
        Err((Data, pos)) => Err(ParseError::new(pos, NotYetImplemented("data"))),
        Err((Trait, pos)) => Err(ParseError::new(pos, NotYetImplemented("trait"))),
    }
}

/// If successful, returns an expr or the direction and location of an arrow
fn parse_catch_keyword<'a>(t: Token<'a>) -> ParseResult<'a, Result<Expr<'a>, (Keyword, FilePos<'a>)>> {
    Ok(match t {
        Token::Word(w, pos) =>
            Ok(if let Ok(n) = w.parse::<usize>() {
                Expr::Val(VToken::new(pos, Value::Nat(n)))
            } else {
                Expr::Val(VToken::new(pos, Value::Sym(w)))
            }),
        Token::SExp(SToken { pos: locable, body }) => {
            let body = parse_tokens(body.0)
                .map_err(|e| ParseError::new(locable.clone(), InSExp(Box::new(e))))?;
            Ok(Expr::SExp(SToken::new(locable, SExp(body))))
        },
        Token::Keyword(kw, pos) => Err((kw, pos)),
    })
}

fn check_params<'a>(t: &Token<'a>) -> ParseResult<'a, ()> {
    let mut used = HashSet::new();
    let mut to_check = vec![t];
    while let Some(next) = to_check.pop() {
        match next {
            Token::Keyword(kw, fp) => return Err(ParseError::new(fp.clone(), MisplacedKeyword(*kw))),
            Token::Word(w, pos) =>
                if !used.insert(w.clone()) {
                    return Err(ParseError::new(pos.clone(), DuplicateLambdaArg(w.clone())))
                },
            Token::SExp(sbody) => to_check.extend(sbody.body.0.iter().rev()),
        }
    }
    Ok(())
}