pub mod lex;
pub mod sources;

use crate::{
    errors::{ParseResult, ParseErrorBody::*, ParseError}, 
    exprs::{Expr, Ident, ExprBody},
    stmts::Stmt,
    values::{Value},
    parsing::lex::{Token, TokenBody::*, Keyword::*}, typing::data::{Kind, Constructor, DataDecl}
};

use self::sources::FilePos;

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

fn parse_stmt<'a>(Token { pos, body }: Token<'a>) -> ParseResult<'a, Stmt<'a>> {
    let ts = match body {
        Keyword(kw) => 
            return Err(ParseError::new(pos, MisplacedKeyword(kw))),
        Literal(c) => 
            return Ok(Stmt::value(pos, Value::Char(c))),
        Word(w) => 
            return Ok(Stmt::value(pos, parse_string(w))),
        SExp(ts) => 
            ts,
    };

    if ts.is_empty() {
        return Ok(Stmt::Expr(Expr { pos, body: ExprBody::SExp(Vec::with_capacity(0)) }));
    }

    if &ts[0].body == &Keyword(Data) {
        return parse_data(ts);
    }

    // from here on, either expr, bind, or lambda
    if ts.len() != 3 { // lambda & bind are both exactly 3 tokens wide
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
        Import | Data | Type =>
            Err(ParseError::new(pos, MisplacedKeyword(kw))),
        Arrow => 
            Ok(Stmt::value(arr_pos, parse_lambda(head, body)?)),
        Backarrow => {
            let (i, e) = parse_backarrow(head, arr_pos, body)?;
            Ok(Stmt::Bind(i, e))
        }
    }
}

fn parse_expr<'a>(t: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    let pos = t.pos;
    match t.body {
        Keyword(kw) => 
            Err(ParseError::new(pos, MisplacedKeyword(kw))),
        Word(w) => 
            Ok(Expr { pos, body: ExprBody::Val(parse_string(w)) }),
        body => {
            match parse_stmt(Token { pos, body })? {
                Stmt::Expr(e) => Ok(e),
                Stmt::Bind(Ident { pos, body }, _) |
                    Stmt::Data(DataDecl { name: Ident { pos, body }, .. })=>
                        Err(ParseError::new(pos, BadBinding(body)))
            }
        }
    }
}

fn parse_backarrow<'a>(head: Token<'a>, arr_pos: FilePos<'a>, body: Token<'a>) -> ParseResult<'a, (Ident<'a>, Expr<'a>)> {
    let head_pos = head.pos;
    match head.body {
        Word(name) => 
            Ok((Ident { pos: arr_pos, body: name }, parse_expr(body)?)),
        Keyword(kw) => 
            Err(ParseError::new(arr_pos, MisplacedKeyword(kw))),
        Literal(_) => 
            return Err(ParseError::new(head_pos, MisplacedLiteral)),
        SExp(ts) => {
            if ts.is_empty() {
                return Err(ParseError::new(arr_pos, MissingIdentifier))
            }
            let mut ts = ts;
            let name = match ts.remove(0) {
                Token { body: Word(w), .. } => 
                    Ok(w),
                Token { pos, body: Keyword(kw) } =>
                    Err(ParseError::new(pos, MisplacedKeyword(kw))),
                Token { pos, body: SExp(_) } =>
                    Err(ParseError::new(pos, MissingIdentifier)),
                Token { pos, body: Literal(_) } =>
                    Err(ParseError::new(pos, MisplacedLiteral)),
            }?;
            let head = Token { 
                pos: arr_pos.clone(), 
                body: SExp(ts) 
            };
            Ok((
                Ident { pos: head_pos, body: name }, 
                Expr { pos: arr_pos, body: ExprBody::Val(parse_lambda(head, body)?) }
            ))
        },
    }
}

fn parse_data<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Stmt<'a>> {
    let n = ts.len();
    let mut ts = ts.into_iter();
    let data_kw_pos = ts.next().unwrap().pos;

    if n < 3 {
        return Err(ParseError::new(data_kw_pos, MisplacedKeyword(Data)))
    }

    let ( Token { body: name, pos: n_pos }
        , kind
        ) = (ts.next().unwrap(), ts.next().unwrap());

    let name = match name {
        Keyword(kw) => 
            return Err(ParseError::new(n_pos, MisplacedKeyword(kw))),
        Word(w) => 
            w,
        Literal(_) => 
            return Err(ParseError::new(n_pos, MisplacedLiteral)),
        SExp(_) => 
            return Err(ParseError::new(n_pos, MissingIdentifier)),
    };

    let kind = parse_kind(kind)?;

    fn parse_constructor<'a>(Token { pos, body }: Token<'a>) -> ParseResult<'a, Constructor> {
        let ts = match body {
            Keyword(kw) => 
                return Err(ParseError::new(pos, MisplacedKeyword(kw))),
            Word(w) => 
                return Err(ParseError::new(pos, BadBinding(w))),
            Literal(_) => 
                return Err(ParseError::new(pos, MisplacedLiteral)),
            SExp(ts) => 
                ts,
        };

        if ts.len() != 3 {
            return Err(ParseError::new(pos, MisplacedSExp));
        }

        let mut ts = ts.into_iter();
        let ( head
            , Token { pos: arr_pos, body: arr_body }
            , body
            ) = (ts.next().unwrap(), ts.next().unwrap(), ts.next().unwrap());

        if arr_body != Keyword(Backarrow) {
            return Err(ParseError::new(pos, MisplacedSExp));
        }
        
        parse_backarrow(head, arr_pos, body)
    }

    let ctors = try_collect(ts.map(parse_constructor))?;

    Ok(Stmt::Data(DataDecl {
        name: Ident { pos: n_pos, body: name },
        kind,
        ctors,
    }))
}

fn parse_kind<'a>(Token { pos, body }: Token<'a>) -> ParseResult<'a, Kind> {
    match body {
        Keyword(Type) =>
            Ok(Kind::Type),
        Keyword(kw) => 
            Err(ParseError::new(pos, MisplacedKeyword(kw))),
        Literal(_) => 
            Err(ParseError::new(pos, MisplacedLiteral)),
        Word(_) => 
            Err(ParseError::new(pos, NotYetImplemented("Custom Kinds"))),
        SExp(ts) => {
            let mut ts = ts.into_iter();
            let arr_pos = match ts.next() {
                None => 
                    return Err(ParseError::new(pos, MisplacedLiteral)),
                Some(Token { body: Keyword(Arrow), pos }) =>
                    pos,
                Some(t) =>
                    return parse_kind(t),
            };

            // reversing order!
            let args = try_collect(ts.map(parse_kind).rev())?;

            if args.len() < 2 {
                return Err(ParseError::new(arr_pos, MisplacedKeyword(Arrow)));
            }

            let mut args = args.into_iter();

            // reversed above!
            let (b, a) = (args.next().unwrap(), args.next().unwrap());

            Ok(args.into_iter().fold(
                Kind::KFn(Box::new(a), Box::new(b)), 
                |acc, k| 
                    Kind::KFn(Box::new(k), Box::new(acc)),
            ))
        },
    }
}

fn parse_lambda<'a>(Token { pos, body }: Token<'a>, body_tkn: Token<'a>) -> ParseResult<'a, Value<'a>> {
    let mut found: Vec<Ident<'a>> = Vec::new();
    match body {
        Literal(_) => 
            return Err(ParseError::new(pos, MisplacedLiteral)),
        Keyword(kw) => 
            return Err(ParseError::new(pos, MisplacedKeyword(kw))),
        Word(w) => {
            if found.iter().any(|i| i.body == w) {
                return Err(ParseError::new(pos, DuplicateLambdaArg(w.clone())));
            } else {
                found.push(Ident { body: w, pos: pos.clone() });
            }
        }
        SExp(ts) => {
            for Token { pos, body } in ts {
                match body {
                    Literal(_) => 
                        return Err(ParseError::new(pos, MisplacedLiteral)),
                    Keyword(kw) => 
                        return Err(ParseError::new(pos, MisplacedKeyword(kw))),
                    Word(w) => {
                        if found.iter().any(|i| i.body == w) {
                            return Err(ParseError::new(pos, DuplicateLambdaArg(w.clone())));
                        } else {
                            found.push(Ident { body: w, pos });
                        }
                    }
                    SExp(_) => {
                        return Err(ParseError::new(pos, MisplacedSExp))
                    }
                }
            }
        }
    }

    if let Some(lst) = found.pop() {
        Ok(
            found.into_iter().rev().fold(
                Value::lam(lst, parse_expr(body_tkn)?), 
                |acc, next| {
                    Value::lam(next, Expr::val(pos.clone(), acc))
                })
        )
    } else {
        Err(ParseError::new(pos, MisplacedSExp))
    }
}

fn parse_string<'a>(w: String) -> Value<'a> {
    w.parse::<usize>().map(Value::Nat).unwrap_or(Value::Sym(w))
}
