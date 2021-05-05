use std::collections::VecDeque;

use crate::{context::{Context, CtxtMap}, evaluator::*};


pub fn form_string(form: &Vec<Token>) -> String {
    form.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(" ")
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub file_pos: FilePos,
}

impl Ident {
    pub fn new(name: String, file_pos: FilePos) -> Self {
        Self { name, file_pos }
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.name, self.file_pos)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Quote(Vec<Token>),
    List(VecDeque<Value>),
    Fn(Vec<Ident>, Option<Ident>, Box<Token>),
    BuiltIn(BuiltInFn),
}

fn make_arg_error(params: &Vec<Ident>, tail_len: usize, fn_name: &Option<String>) -> Error {
    let f_name = if let Some(s) = fn_name { s.clone() } else { format!("<anon func>") };
    Error::ArgError { f_name, expected: params.len(), recieved: tail_len }
}

impl Value {
    pub fn eval(&self, ctxt: &Context, r_tail: Result<Vec<Token>, (Vec<Value>, FilePos)>, fn_name: &Option<String>) -> EvalResult<Value> {
        match self {
            Value::Fn(params, op_rest, body) => {
                let tail = match r_tail {
                    Ok(ts) => eval_all(ctxt, ts)?,
                    Err((vs, _)) => vs, 
                };

                let tail_len = tail.len();
                if tail_len < params.len() || (op_rest.is_none() && tail_len > params.len()) {                    
                    return Err(make_arg_error(params, tail_len, fn_name))
                }
        
                let mut iter = tail.into_iter();
                let mut data = CtxtMap::with_capacity(params.len() + 1);
                for ident in params.iter() {
                    let t = iter.next().expect("simple eval check failed");
                    let v = (t, Some(ident.file_pos));
                    data.insert(ident.name.clone(), v);
                }
                
                if let Some(rest) = op_rest {
                    let qut = Value::Quote(iter.map(|t| Token::from_value(t, rest.file_pos)).collect());
                    let v = (qut, Some(rest.file_pos));
                    data.insert(rest.name.clone(), v);
                }
        
                let next = ctxt.chain(data);
                body.as_ref().eval(&next)
            },
            Value::BuiltIn(bifn) => {
                let tail = match r_tail {
                    Ok(ts) => ts,
                    Err((vs, file_pos)) => vs.into_iter().map(|v| Token::from_value(v, file_pos)).collect(),
                };
                (bifn.f)(ctxt, tail)
            },
            Value::Quote(form) => run_form(form, ctxt),
            v => {
                let len = match r_tail {
                    Ok(ts) => ts.len(),
                    Err((vs, _)) => vs.len(),
                };
                if len == 0 {
                    Ok(v.clone())
                } else {
                    Err(Error::ValueError(v.clone(), format!("not a function")))
                }
            }
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
            Value::List(list) =>
                write!(f, "'({})", list.iter().map(|v| format!("{}", v)).collect::<Vec<String>>().join(" "))
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        use Value::*;
        match (self, other) {
            (Unit, Unit) => true,
            (Unit, _) => false,
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
            (Quote(x), List(y)) =>
                x.len() == y.len() && x.iter().zip(y)
                    .all(|(a, b)| if let Expr::Lit(av) = &a.expr { av == b } else { false }),
            (Quote(_), _) => false,
            (List(x), List(y)) => 
                x.len() == y.len() && x.iter().zip(y).all(|(a, b)| a == b),
            (List(x), Quote(y)) =>
                x.len() == y.len() && x.iter().zip(y)
                    .all(|(a, b)| if let Expr::Lit(bv) = &b.expr { bv == a } else { false }),
            (List(_), _) => false,
        }
    }
}
