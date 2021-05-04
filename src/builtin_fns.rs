use crate::{context::Context, evaluator::{Error::*, EvalResult, Token, Value, eval_all}};

use Value::*;

pub fn add(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
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

    Ok(match *out {
        Ok(x) => Int(x),
        Err(x) => Float(x),
    })
}

pub fn sub(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    if let Some((h, t)) = tokens.split_first() {
        if t.len() > 0 {
            return match h.eval(ctxt)? {
                Int(n) => match add(ctxt, Vec::from(t))? {
                    Int(x) => Ok(Int(n - x)),
                    Float(x) => Ok(Float(n as f64 - x)),
                    x => Err(ValueError(x, format!("(+) returned non-numeric"))),
                },
                Float(n) => match add(ctxt, Vec::from(t))? {
                    Int(x) => Ok(Float(n - x as f64)),
                    Float(x) => Ok(Float(n - x)),
                    x => Err(ValueError(x, format!("(+) returned non-numeric"))),
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

pub fn ap(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let (first, rest) = 
        tokens.split_first()
            .ok_or_else(|| ArgError { f_name: format!("ap"), recieved: 0, expected: 1 })?;
    let head = first.eval(ctxt)?;
    let mut tail = Vec::with_capacity(tokens.len());
    for t in Vec::from(rest) {
        if let Quote(form) = t.eval(ctxt)? {
            tail.extend(form.into_iter());
        } else {
            tail.push(t);
        }
    }
    head.eval(ctxt, tail, &first.expr)
}

pub fn if_(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    if tokens.len() != 3 {
        return Err(ArgError { f_name: format!("if"), recieved: tokens.len(), expected: 3 })
    }

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