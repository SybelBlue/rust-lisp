pub mod lex;
pub mod sources;

use std::collections::HashSet;

use crate::{
    errors::{ParseResult, ParseErrorBody::*, ParseError}, 
    exprs::{Stmt, Expr, values::{Value, VToken}, Ident}
};

use self::lex::{Token, TokenBody::*, Keyword::*};

fn try_collect<T, E, I: Iterator<Item=Result<T, E>>>(itr: I) -> Result<Vec<T>, E> {
    let mut out = Vec::new();
    for t in itr {
        out.push(t?);
    }
    Ok(out)
}


pub fn parse<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Stmt<'a>>> {
    try_collect(ts.into_iter().map(parse_stmt))
}

fn parse_stmt<'a>(t: Token<'a>) -> ParseResult<'a, Stmt<'a>> {
    let pos = t.pos;
    let ts = match t.body {
        Keyword(kw) => 
            return Err(ParseError::new(pos, MisplacedKeyword(kw))),
        Word(w) => 
            return Ok(Stmt::value(pos, parse_string(w))),
        SExp(crate::exprs::SExp(ts)) => ts,
    };

    if ts.len() != 3 {
        let es = try_collect(ts.into_iter().map(parse_expr))?;
        return Ok(Stmt::sexp(pos, es));
    }

    let mut ts = ts.into_iter();

    let (head, arr, body) = (ts.next().unwrap(), ts.next().unwrap(), ts.next().unwrap());

    let (arr_pos, kw) = if let Token { pos, body: Keyword(kw) } = arr {
        (pos, kw)
    } else {
        let body = try_collect(vec![head, arr, body].into_iter().map(parse_expr))?;
        return Ok(Stmt::sexp(pos, body));
    };

    match kw {
        Arrow => 
            Ok(Stmt::value(arr_pos, parse_lambda(head, body)?)),
        Backarrow => {
            let head_pos = head.pos;
            match head.body {
                Word(name) => 
                    Ok(Stmt::Bind(Ident { pos: arr_pos, body: name }, parse_expr(body)?)),
                Keyword(kw) => 
                    Err(ParseError::new(arr_pos, MisplacedKeyword(kw))),
                SExp(crate::exprs::SExp(ts)) => {
                    if ts.is_empty() {
                        return Err(ParseError::new(arr_pos, MissingBindingIdentifier))
                    }
                    let mut ts = ts;
                    let name = match ts.remove(0) {
                        Token { body: Word(w), .. } => 
                            Ok(w),
                        Token { pos, body: Keyword(kw) } =>
                            Err(ParseError::new(pos, MisplacedKeyword(kw))),
                        Token { pos, body: SExp(_) } =>
                            Err(ParseError::new(pos, MissingBindingIdentifier)),
                    }?;
                    let head = Token { 
                        pos: arr_pos.clone(), 
                        body: SExp(crate::exprs::SExp(ts)) 
                    };
                    Ok(Stmt::Bind(
                        Ident { pos: head_pos, body: name }, 
                        Expr::Val(VToken { pos: arr_pos, body: parse_lambda(head, body)? })
                    ))
                },
            }
        },
    }
}

fn parse_expr<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    let pos = t.pos;
    match t.body {
        Keyword(kw) => 
            Err(ParseError::new(pos, MisplacedKeyword(kw))),
        Word(w) => 
            Ok(Expr::Val(VToken { pos, body: parse_string(w) })),
        body => {
            let t = Token { pos, body };
            match parse_stmt(t)? {
                Stmt::Expr(e) => Ok(e),
                Stmt::Bind(Ident { pos, body }, _) =>
                    Err(ParseError::new(pos, BadBinding(body)))
            }
        }
    }
}

fn parse_lambda<'a>(head: Token<'a>, body: Token<'a>) -> ParseResult<'a, Value<'a>> {
    let mut found = HashSet::new();
    let mut to_check = vec![&head];
    while let Some(chk) = to_check.pop() {
        match &chk.body {
            Keyword(kw) => 
                return Err(ParseError::new(chk.pos.clone(), MisplacedKeyword(*kw))),
            Word(w) => {
                if found.contains(w) {
                    return Err(ParseError::new(chk.pos.clone(), DuplicateLambdaArg(w.clone())));
                } else {
                    found.insert(w.clone());
                }
            }
            SExp(crate::exprs::SExp(ts)) => {
                to_check.extend(ts);
            }
        }
    }
    Ok(Value::lam(parse_expr(head)?, parse_expr(body)?))
}

fn parse_string<'a>(w: String) -> Value<'a> {
    w.parse::<usize>().map(Value::Nat).unwrap_or(Value::Sym(w))
}
