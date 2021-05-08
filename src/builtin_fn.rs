use std::collections::{HashMap, VecDeque};

use crate::{
    context::{Context, CtxtMap, CtxtMapValue}, 
    evaluator::{
        *, 
        expr::Expr, 
        result::{*, Error::*}, 
        token::Token, 
        value::{Value::{self, *}
    }
}};

#[derive(Clone)]
pub struct BuiltInFn {
    pub name: String, 
    pub f: fn(&Context<'_>, Vec<Token>) -> EvalResult<Value>,
}

impl BuiltInFn {
    pub fn new(name: &str, f: fn(&Context<'_>, Vec<Token>) -> EvalResult<Value>) -> Value {
        Value::BuiltIn(Self::simple(String::from(name), f))
    }

    pub fn simple(name: String, f: fn(&Context<'_>, Vec<Token>) -> EvalResult<Value>) -> Self {
        BuiltInFn { name, f }
    }
}

impl std::fmt::Debug for BuiltInFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BIBody({})", self.name)
    }
}

impl std::cmp::PartialEq for BuiltInFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

fn just_add(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Result<i64, f64>> {
    let vals = eval_all(ctxt, tokens)?;

    let out: &mut Result<i64, f64> = &mut Ok(0);
    for v in vals {
        match v {
            Int(n) => match out {
                Ok(x) => *x += n,
                Err(x) => *x += n as f64,
            },
            Float(n) => match out {
                Ok(x) => *out = Err(n + *x as f64),
                Err(x) => *x += n,
            },
            x => return Err(ValueError(x, format!("(+) takes only numeric arguments"))),
        }
    }

    Ok(*out)
}

pub fn add(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    Ok(match just_add(ctxt, tokens)? {
        Ok(x) => Int(x),
        Err(x) => Float(x),
    })
}

pub fn sub(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    if let Some((h, t)) = tokens.split_first() {
        if t.len() > 0 {
            return match h.eval(ctxt)? {
                Int(n) => match just_add(ctxt, Vec::from(t))? {
                    Ok(x) => Ok(Int(n - x)),
                    Err(x) => Ok(Float(n as f64 - x)),
                },
                Float(n) => match just_add(ctxt, Vec::from(t))? {
                    Ok(x) => Ok(Float(n - x as f64)),
                    Err(x) => Ok(Float(n - x)),
                },
                v => Err(ValueError(v, format!("(-) takes only numeric arguments"))),
            }
        }
    }
    Err(ArgError { f_name: format!("(-)"), recieved: tokens.len(), expected: 2 })
}

pub fn eq(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let vals = eval_all(ctxt, tokens)?;
    if let Some(u) = vals.get(0) {
        if let Some(v) = vals.get(1) {
            let res = Int(if *u == *v { 1 } else { 0 });
            return Ok(res)
        }
    }
    Err(ArgError { f_name: format!("(=)"), recieved: vals.len(), expected: 2 })
}

pub fn unpack(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let (first, rest) = 
        tokens.split_first()
            .ok_or_else(|| ArgError { f_name: format!("unpack"), recieved: 0, expected: 1 })?;
    let head = first.eval(ctxt)?;
    let mut tail = Vec::with_capacity(tokens.len());
    for t in Vec::from(rest) {
        let v = t.eval(ctxt)?;
        if let Quote(form) = v {
            tail.extend(form.into_iter());
        } else {
            tail.push(Token::new(Expr::Lit(v), t.file_pos));
        }
    }
    head.eval(ctxt, Ok(tail), &first.expr.get_var_name())
}

fn check_arg_count(f_name: &str, tokens: &Vec<Token>, n: usize) -> EvalResult<()> {
    if tokens.len() != n {
        Err(ArgError { f_name: String::from(f_name), recieved: tokens.len(), expected: n })
    } else {
        Ok(())
    }
}

fn require_two<'a>(f_name: &str, tokens: &'a Vec<Token>) -> EvalResult<(&'a Token, &'a Token)> {
    check_arg_count(f_name, tokens, 2)?;
    let f = tokens.first().ok_or_else(|| InternalError(format!("{} check failed! (0)", f_name)))?;
    let s = tokens.last().ok_or_else(|| InternalError(format!("{} check failed! (1)", f_name)))?;
    Ok((f, s))
}

pub fn cons(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let (f, s) = require_two("cons", &tokens)?;
    let first = f.eval(ctxt)?;
    match s.eval(ctxt)? {
        Quote(tail) => {
            let mut v = Vec::with_capacity(tail.len() + 1);
            v.push(Token::from_value(first, f.file_pos));
            v.extend(tail);
            Ok(Quote(v))
        },
        List(tail) => {
            let mut vd = tail.clone();
            vd.push_front(first);
            Ok(List(vd))
        },
        v => Err(ValueError(v, format!("Second arg to cons must be quote or list")))
    }
}

pub fn append(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let (f, s) = require_two("append", &tokens)?;
    let first = f.eval(ctxt)?;
    match s.eval(ctxt)? {
        Quote(tail) => {
            let mut v = Vec::with_capacity(tail.len() + 1);
            v.extend(tail);
            v.push(Token::from_value(first, f.file_pos));
            Ok(Quote(v))
        },
        List(tail) => {
            let mut vd = tail.clone();
            vd.push_back(first);
            Ok(List(vd))
        },
        v => Err(ValueError(v, format!("Second arg to append must be quote or list")))
    }
}

pub fn ap(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let (f, s) = require_two("ap", &tokens)?;
    let first = f.eval(ctxt)?;
    match s.eval(ctxt)? {
        Quote(tail) => first.eval(ctxt, Ok(tail), &f.expr.get_var_name()),
        List(tail) => {
            first.eval(ctxt, Err((Vec::from(tail), f.file_pos)), &f.expr.get_var_name())
        },
        v => Err(ValueError(v, format!("Second arg to ap must be quote")))
    }
}

pub fn id(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    check_arg_count("id", &tokens, 1)?;
    tokens.first().expect("id check failed").eval(ctxt)
}

pub fn if_(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    check_arg_count("if", &tokens, 3)?;

    let first = tokens.first().expect("prev if check failed! (0)");
    let head = first.eval(ctxt)?;
    let truthy = match head {
        Int(x) => Ok(x != 0),
        Float(x) => Ok(x != 0f64),
        v => Err(ValueError(v, format!("First if arg must be numeric"))),
    }?;
    
    tokens.get(if truthy { 1 } else { 2 })
        .expect("prev if check failed! (1)")
        .eval(ctxt)
}

pub fn assert(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    check_arg_count("assert", &tokens, 1)?;

    let first = tokens.first().expect("prev assert check failed! (0)");
    let head = first.eval(ctxt)?;
    let truthy = match head {
        Int(x) => Ok(x != 0),
        Float(x) => Ok(x != 0f64),
        v => Err(ValueError(v, format!("First assert arg must be numeric"))),
    }?;

    if !truthy {
        panic!(format!("AssertError: falsy {}", first))
    }

    Ok(Unit)
}

pub fn quote(_: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    Ok(Quote(tokens))
}

pub fn list(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    eval_all(ctxt, tokens).map(VecDeque::from).map(List)
}

pub fn p_match(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let t_len = tokens.len();
    let mut t_iter = tokens.into_iter();
    let val = t_iter.next()
        .ok_or_else(|| Error::ArgError { f_name: format!("match"), recieved: 0, expected: t_len })?
        .eval(ctxt)?;
    
    while let Some(template) = t_iter.next() {
        let branch = t_iter.next()
            .ok_or_else(|| Error::ArgError { f_name: format!("match"), recieved: t_len, expected: t_len + 1 })?;
        match matches_(ctxt, &val, template)? {
            MatchResult::NoMatch => {},
            MatchResult::Matches => {},
            MatchResult::NewCtxt(_) => {},
        }
    }

    todo!()
}

enum MatchResult {
    NoMatch,
    Matches,
    NewCtxt(HashMap<String, CtxtMapValue>),
}

fn matches_<'a>(ctxt: &'a Context<'a>, val: &Value, template: Token) -> EvalResult<MatchResult> {
    use MatchResult::*;
    match template.expr {
        Expr::Var(x) => {
            let mut next = CtxtMap::with_capacity(1);
            next.insert(x, (val.clone(), Some(template.file_pos)));
            Ok(NewCtxt(next))
        },
        Expr::Lit(v) => { 
            Ok(if &v == val {
                Matches
            } else {
                NoMatch
            })
         },
        Expr::Form(form) => { 
            match val {
                Quote(v) => {
                    if v.len() != form.len() { 
                        Ok(NoMatch)
                    } else {
                        let mut next = CtxtMap::with_capacity(v.len());
                        for (tk, to) in v.iter().zip(form) {
                            match matches_(ctxt, &tk.eval(ctxt)?, to)? {
                                NoMatch => return Ok(NoMatch),
                                Matches => {},
                                NewCtxt(x) => next.extend(x), // todo! can redef variables!
                            }
                        }
                        Ok(NewCtxt(next))
                    }
                },
                List(v) => {
                    if v.len() != form.len() { 
                        Ok(NoMatch)
                    } else {
                        let mut next = CtxtMap::with_capacity(v.len());
                        for (va, to) in v.iter().zip(form) {
                            match matches_(ctxt, va, to)? {
                                NoMatch => return Ok(NoMatch),
                                Matches => {},
                                NewCtxt(x) => next.extend(x), // todo! can redef variables!
                            }
                        }
                        Ok(NewCtxt(next))
                    }
                },
                _ => Ok(NoMatch)
            }
        },
        Expr::Def(_, _) | Expr::Import(_, _) => Err(ValueError(Unit, format!("matches requires a literal, variable, or form template")))
    }
}