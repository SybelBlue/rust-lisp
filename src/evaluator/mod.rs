use crate::{
    context::Context, 
    parser::{ParseStream, parse_all}, 
    evaluator::{token::Token, value::*, result::*}
};

pub mod value;
pub mod result;
pub mod token;
pub mod expr;

pub fn exec(s: String) -> (Vec<EvalResult<Value>>, Context<'static>) {
    let mut ctxt = Context::new();
    let out = exec_using(s, &mut ctxt, &None);
    (out, ctxt)
}

pub fn exec_using(s: String, ctxt: &mut Context, namespace: &Option<String>) -> Vec<EvalResult<Value>> {
    let op_exprs = parse_all(&mut ParseStream::new(&mut s.chars().peekable()));
    let mut out = Vec::new();
    for e in op_exprs {
        match e {
            Err(s) => out.push(Err(Error::ParseError(s))),
            Ok(x) => out.push(x.exec(ctxt, false, namespace))
        }
    }
    out
}

pub fn eval_all(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Vec<Value>> {
    let mut out = Vec::new();
    for e in tokens {
        out.push(e.eval(ctxt)?);
    }
    Ok(out)
}