use crate::{context::Context, evaluator::{Error::*, EvalResult, Token, Value, eval_all}};


pub fn add(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Value> {
    let vals = eval_all(ctxt, tokens)?;
    use Value::*;

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
    use Value::*;
    if let Some((h, t)) = tokens.split_first() {
        if t.len() > 0 {
            return match h.eval(ctxt)? {
                Int(n) => match add(ctxt, Vec::from(t))? {
                    Int(x) => Ok(Int(n - x)),
                    Float(x) => Ok(Float(n as f64 - x)),
                    x => Err(ValueError(x, format!("Addition returned non-numeric"))),
                },
                Float(n) => match add(ctxt, Vec::from(t))? {
                    Int(x) => Ok(Float(n - x as f64)),
                    Float(x) => Ok(Float(n - x)),
                    x => Err(ValueError(x, format!("Addition returned non-numeric"))),
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
            return Ok(Value::Int(if *u == *v { 1 } else { 0 }))
        }
    }
    Err(ArgError { f_name: format!("(==)"), recieved: vals.len(), expected: 2 })
}

pub fn ap(ctxt: &Context<'_>, vals: Vec<Token>) -> EvalResult<Value> {
    let first = vals.first().ok_or_else(|| ArgError { f_name: format!("ap"), recieved: 0, expected: 1 })?;
    todo!()
}