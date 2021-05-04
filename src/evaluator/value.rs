use std::collections::HashMap;

use crate::context::Context;

use super::*;


pub fn form_string(form: &Vec<Token>) -> String {
    form.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(" ")
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Quote(Vec<Token>),
    Fn(Vec<Ident>, Option<Ident>, Box<Token>),
    BuiltIn(BuiltInFn),
}

fn make_arg_error(params: &Vec<Ident>, tail: &Vec<Token>, src_expr: &Expr) -> Error {
    let f_name = if let Some(s) = src_expr.get_var_name() { s.clone() } else { format!("<anon func>") };
    Error::ArgError { f_name, expected: params.len(), recieved: tail.len() }
}

impl Value {
    pub fn eval(&self, ctxt: &Context, tail: Vec<Token>, src_expr: &Expr) -> EvalResult<Value> {
        match self {
            Value::Fn(params, op_rest, body) => {
                if tail.len() < params.len() || (op_rest.is_none() && tail.len() > params.len()) {                    
                    return Err(make_arg_error(params, &tail, src_expr))
                }
                let mut data = HashMap::with_capacity(params.len() + 1);
                let mut iter = tail.iter();
                for ident in params.iter() {
                    let t = iter.next().ok_or_else(|| make_arg_error(params, &tail, src_expr))?;
                    let t_val = t.eval(ctxt)?;
                    let v = (t_val, Some(ident.file_pos));
                    data.insert(ident.name.clone(), v);
                }
                
                if let Some(rest) = op_rest {
                    let qut = Value::Quote(iter.map(|t| t.clone()).collect());
                    let v = (qut, Some(rest.file_pos));
                    data.insert(rest.name.clone(), v);
                }

                let next = ctxt.chain(data);
                body.as_ref().eval(&next)
            },
            Value::BuiltIn(bifn) => (bifn.f)(ctxt, tail),
            Value::Quote(form) => run_form(form, ctxt),
            v if tail.is_empty() => Ok(v.clone()), 
            v => Err(Error::ValueError(v.clone(), format!("not a function"))),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::Fn(args, Some(_), _) => write!(f, "<({}+n)-ary func>", args.len()),
            Value::Fn(args, None, _) => write!(f, "<{}-ary func>", args.len()),
            Value::BuiltIn(bifn) => write!(f, "<builtin func {}>", bifn.name),
            Value::Quote(form) =>
                if let Some((h, &[])) = form.split_first() {
                    write!(f, "'{}", h)
                } else {
                    write!(f, "'({})", form_string(form))
                }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Unit, x) => x == &Unit,
            (Int(x), Int(y)) => x == y,
            (Int(_), _) => false,
            (Float(x), Float(y)) => x == y,
            (Float(_), _) => false,
            (Fn(_, r, x), 
                Fn(_, s, y)) => 
                    r == s && x.expr == y.expr,
            (Fn(_, _, _), _) => false, 
            (BuiltIn(x), 
                BuiltIn(y)) => x == y,
            (BuiltIn(_), _) => false,
            (Quote(x), Quote(y)) => 
                x.len() == y.len() && x.iter().zip(y).all(|(a, b)| a.expr == b.expr),
            (Quote(_), _) => false,
        }
    }
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
