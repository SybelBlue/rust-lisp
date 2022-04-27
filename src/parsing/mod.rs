pub mod lex;
pub mod sources;

use std::{collections::HashSet, vec::IntoIter};

use crate::{exprs::{Expr, values::{Value, VToken}, SToken, SExp, Stmt, Ident}, errors::{ParseResult, ParseError}};

use crate::errors::ParseErrorBody::*;
use crate::parsing::lex::Keyword::*;

use self::{lex::{Token, Keyword, TokenBody}, sources::FilePos};


pub fn parse<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Stmt<'a>>> {
    if ts.is_empty() { return Ok(Vec::new()) }

    let mut ts = ts.into_iter();
    
    let fst_tkn = ts.next().unwrap();

    let snd = if let Some(t) = ts.next() {
        parse_catch_keyword(t)?
    } else {
        return Ok(vec![Stmt::Expr(parse_simple(fst_tkn)?)]);
    };

    match snd {
        Err((Backarrow, arrpos)) => {
            match fst_tkn.body {
                TokenBody::Keyword(_) => 
                    Err(ParseError::new(arrpos, NotYetImplemented("Backarrow bind"))),
                TokenBody::Word(body) => 
                    Ok(vec![Stmt::Bind(Ident { body, pos: fst_tkn.pos }, get_lambda_body(arrpos, ts)?)]),
                TokenBody::SExp(_) => 
                    Err(ParseError::new(arrpos, NotYetImplemented("Backarrow bind"))),
            }
        }
        Err((Arrow, pos)) => {
            check_params(&fst_tkn)?;
            let params = parse_simple(fst_tkn)?;
            let body = get_lambda_body(pos.clone(), ts)?;
            Ok(vec![Stmt::value(pos, Value::lam(params, body))])
        }
        Ok(snd) => {
            let mut out = vec![
                Stmt::Expr(parse_simple(fst_tkn)?), 
                Stmt::Expr(snd)
            ];
            for t in ts {
                out.push(Stmt::Expr(parse_simple(t)?));
            }
            Ok(out)
        }
    }
}

fn get_lambda_body<'a>(arrpos: FilePos<'a>, ts: IntoIter<Token<'a>>) -> ParseResult<'a, Expr<'a>> {
    let mut ts = ts;
    if let Some(t) = ts.next() {
        if ts.next().is_none() {
            parse_simple(t)
        } else {
            Err(ParseError::new(arrpos, ExtraLambdaBody))
        }
    } else {
        Err(ParseError::new(arrpos, MissingLambdaBody))
    }
}

fn parse_exprs<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Expr<'a>>> {
    let mut out = Vec::new();
    for t in parse(ts)? {
        match t {
            Stmt::Expr(e) => out.push(e),
            Stmt::Bind(Ident { pos, .. }, _) => 
                return Err(ParseError::new(pos, MisplacedKeyword(Backarrow))),
        }
    }
    Ok(out)
}

fn parse_simple<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    parse_catch_keyword(t)?.map_err(|(kw, fp)| 
        ParseError::new(fp, MisplacedKeyword(kw)))
}

/// If successful, returns an expr or the direction and location of an arrow
fn parse_catch_keyword<'a>(Token { pos, body }: Token<'a>) -> ParseResult<'a, Result<Expr<'a>, (Keyword, FilePos<'a>)>> {
    Ok(match body {
        TokenBody::Word(w) =>
            Ok(if let Ok(n) = w.parse::<usize>() {
                Expr::Val(VToken::new(pos, Value::Nat(n)))
            } else {
                Expr::Val(VToken::new(pos, Value::Sym(w)))
            }),
        TokenBody::SExp(SExp(body)) => {
            let body = parse_exprs(body)
                .map_err(|e| ParseError::new(pos.clone(), InSExp(Box::new(e))))?;
            Ok(Expr::SExp(SToken::new(pos, SExp(body))))
        },
        TokenBody::Keyword(kw) => Err((kw, pos)),
    })
}

fn check_params<'a>(t: &Token<'a>) -> ParseResult<'a, ()> {
    let mut used = HashSet::new();
    let mut to_check = vec![t];
    while let Some(Token { pos, body }) = to_check.pop() {
        match body {
            TokenBody::Keyword(kw) => return Err(ParseError::new(pos.clone(), MisplacedKeyword(*kw))),
            TokenBody::Word(w) =>
                if !used.insert(w.clone()) {
                    return Err(ParseError::new(pos.clone(), DuplicateLambdaArg(w.clone())))
                },
            TokenBody::SExp(SExp(sbody)) => to_check.extend(sbody.iter().rev()),
        }
    }
    Ok(())
}