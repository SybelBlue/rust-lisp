use std::collections::VecDeque;

use crate::{builtin_fn::BuiltInFn, context::{Context, CtxtMap}, evaluator::{*, value::Value::*}};


pub fn form_string(form: &[Token]) -> String {
    form.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(" ")
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
    Macro(Vec<(bool, Ident)>, Option<Ident>, Box<Token>),
    BuiltIn(BuiltInFn),
}

fn make_arg_error(params: &Vec<Ident>, tail_len: usize, fn_name: &Option<String>) -> Error {
    let f_name = if let Some(s) = fn_name { s.clone() } else { format!("<anon func>") };
    Error::ArgError { f_name, expected: params.len(), recieved: tail_len }
}

#[inline]
fn run_fn(
        ctxt: &Context, 
        tail: Vec<Value>, 
        fn_name: &Option<String>,
        params: &Vec<Ident>, 
        op_rest: &Option<Ident>, 
        body: &Box<Token>)
         -> EvalResult<Value> {
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
        let qut = List(iter.collect());
        let v = (qut, Some(rest.file_pos));
        data.insert(rest.name.clone(), v);
    }

    let next = ctxt.chain(data);
    body.as_ref().eval(&next)
}

impl Value {
    pub fn eval(&self, ctxt: &Context, r_tail: Result<Vec<Token>, (Vec<Value>, FilePos)>, fn_name: &Option<String>) -> EvalResult<Value> {
        match self {
            Macro(macro_params, op_rest, body) => {
                let macro_params = macro_params.clone();
                let tail = match r_tail {
                    Ok(ts) => {
                        let mut tail = Vec::new();
                        for (t, (b, _)) in ts.into_iter().zip(macro_params.iter()) {
                            tail.push(if *b {
                                Quote(vec![t])
                            } else {
                                t.eval(ctxt)?
                            });
                        }
                        tail
                    },
                    Err((vs, _)) => vs,
                };
                let params = macro_params.into_iter().map(|(_, i)| i).collect();
                run_fn(ctxt, tail, fn_name, &params, op_rest, body)
            },
            Fn(params, op_rest, body) => {
                let tail = match r_tail {
                    Ok(ts) => eval_all(ctxt, ts)?,
                    Err((vs, _)) => vs, 
                };
                run_fn(ctxt, tail, fn_name, params, op_rest, body)
            },
            BuiltIn(bifn) => {
                let tail = match r_tail {
                    Ok(ts) => ts,
                    Err((vs, file_pos)) => vs.into_iter().map(|v| Token::from_value(v, file_pos)).collect(),
                };
                (bifn.f)(ctxt, tail)
            },
            Quote(form) => run_form(form, ctxt),
            Unit | Int(_) | Float(_) | List(_) => {
                let len = match r_tail {
                    Ok(ts) => ts.len(),
                    Err((vs, _)) => vs.len(),
                };
                if len == 0 {
                    Ok(self.clone())
                } else {
                    Err(Error::ValueError(self.clone(), format!("not a function")))
                }
            }
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unit => write!(f, "()"),
            Int(x) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", x),
            Fn(args, Some(_), _) => write!(f, "<({}+n)-ary func>", args.len()),
            Fn(args, None, _) => write!(f, "<{}-ary func>", args.len()),
            Macro(args, Some(_), _) => write!(f, "<({}+n)-ary macro>", args.len()),
            Macro(args, None, _) => write!(f, "<{}-ary macro>", args.len()),
            BuiltIn(bifn) => write!(f, "<builtin func {}>", bifn.name),
            Quote(form) =>
                if let Some((h, &[])) = form.split_first() {
                    write!(f, "'{}", h)
                } else {
                    write!(f, "'({})", form_string(form))
                }
            List(list) =>
                write!(f, "'({})", list.iter().map(|v| format!("{}", v)).collect::<Vec<String>>().join(" "))
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
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
            (Macro(_, r, x), 
                Macro(_, s, y)) => 
                    r == s && x.expr == y.expr,
            (Macro(_, _, _), _) => false, 
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
