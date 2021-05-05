use crate::{
    context::Context, 
    evaluator::{*,
        result::{*, Error::*}, 
        value::Value::{self, *}}
};

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
    Err(ArgError { f_name: format!("(==)"), recieved: vals.len(), expected: 2 })
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
            Ok(List(tail))
        },
        v => Err(ValueError(v, format!("Second arg to cons must be quote or list")))
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

pub fn quote(_: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    Ok(Value::Quote(tokens))
}