pub mod lex;
pub mod sources;

use std::collections::HashSet;
use std::vec::IntoIter;

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
        Err((kw@Trait, pos)) | Err((kw@Data, pos)) => {
            Err(ParseError::new(pos, MisplacedKeyword(kw)))
        }
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
        Ok(snd) => {
            let fst = match parse_catch_keyword(fst_tkn)? {
                Ok(e) => e,
                Err((Arrow, pos)) => {
                    Expr::Val(VToken::new(pos, Value::Sym(String::from("->"))))
                }
                Err((Backarrow, pos)) => {
                    return Err(ParseError::new(pos, MisplacedKeyword(Backarrow)))
                }
                Err((Data, pos)) => {
                    return parse_data_decl(pos, snd, ts);
                },
                Err((Trait, pos)) => {
                    return Err(ParseError::new(pos, NotYetImplemented("trait")));
                },
            };
            let mut out = vec![fst, snd];
            for t in ts {
                out.push(parse_simple(t)?);
            }
            Ok(out)
        }
    }
}

fn parse_data_decl<'a>(pos: FilePos<'a>, snd: Expr<'a>, ts: IntoIter<Token<'a>>) -> ParseResult<'a, Vec<Expr<'a>>> {
    fn parse_ident_type_pair<'a>(t: Token<'a>) -> ParseResult<'a, (String, Expr<'a>)> {
        if let Token::SExp(SToken { body: SExp(v), pos }) = t {
            let mut ts = v.into_iter();
            match (ts.next(), ts.next()) {
                (_, None) => 
                    Err(ParseError::new(pos, NotYetImplemented("data variant defaults"))),
                (Some(Token::Word(name, _)), Some(snd)) =>
                    Ok((name.clone(), parse_simple(snd)?)),
                _ =>
                    Err(ParseError::new(pos, BadDataVariant)),
            }
        } else {
            Err(ParseError::new(t.pos(), NotYetImplemented("data variant defaults")))
        }
    }

    let name = if let Expr::Val(VToken { body: Value::Sym(s), .. }) = snd {
        s
    } else {
        return Err(ParseError::new(pos, BadDataIdentifier))
    };

    let mut variants = Vec::new();
    for t in ts {
        variants.push(parse_ident_type_pair(t)?);
    }

    todo!()
}

/// Any keyword results in an Error
fn parse_simple<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    parse_catch_keyword(t)?.map_err(|(kw, fp)| 
        ParseError::new(fp, MisplacedKeyword(kw)))
}

/// Allows only Arrow as "->" because -> :: (-> Type Type Type)
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
            Token::Word(w, pos) => {
                if !used.insert(w.clone()) {
                    return Err(ParseError::new(pos.clone(), DuplicateLambdaArg(w.clone())))
                }
            }
            Token::SExp(sbody) => to_check.extend(sbody.body.0.iter().rev()),
        }
    }
    Ok(())
}