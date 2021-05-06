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

fn make_arg_error(params_len: usize, tail_len: usize, fn_name: &Option<String>) -> Error {
    let f_name = if let Some(s) = fn_name { s.clone() } else { format!("<anon func>") };
    Error::ArgError { f_name, expected: params_len, recieved: tail_len }
}

#[inline]
fn arg_check(
        tail_len: usize, 
        fn_name: &Option<String>,
        params_len: usize, 
        op_rest: &Option<Ident>)
         -> EvalResult<()> {
    if tail_len < params_len || (op_rest.is_none() && tail_len > params_len) {                    
        Err(make_arg_error(params_len, tail_len, fn_name))
    } else {
        Ok(())
    }
}

#[inline]
fn into_tokens(vs: Vec<Value>, file_pos: FilePos) -> Vec<Token> {
    vs.into_iter().map(|v| Token::from_value(v, file_pos)).collect()
}

#[inline]
fn run_fn(
        ctxt: &Context, 
        tail: Vec<Token>, 
        fn_name: &Option<String>,
        params: &Vec<Ident>, 
        op_rest: &Option<Ident>, 
        body: &Box<Token>)
         -> EvalResult<Value> {
    arg_check(tail.len(), fn_name, params.len(), op_rest)?;
    let mut next = Context::chain(&ctxt, CtxtMap::with_capacity(params.len() + 1));
    let mut t_iter = tail.into_iter();
    for p in params.into_iter() {
        let t = t_iter.next().expect("fn check failed");
        next.put(p.clone(), t.eval(ctxt)?, false, &None)?;
    }
    if let Some(r) = op_rest {
        next.put(r.clone(), Quote(t_iter.collect()), false, &None)?;
    }

    body.as_ref().eval(&next)
}

impl Value {
    pub fn eval(&self, ctxt: &Context, r_tail: Result<Vec<Token>, (Vec<Value>, FilePos)>, fn_name: &Option<String>) -> EvalResult<Value> {
        match self {
            Macro(macro_params, op_rest, body) => {
                let macro_params = macro_params.clone();
                match r_tail {
                    Ok(ts) => {
                        arg_check(ts.len(), fn_name, macro_params.len(), op_rest)?;
                        let mut next = Context::chain(&ctxt, CtxtMap::with_capacity(macro_params.len() + 1));
                        let mut t_iter = ts.into_iter();
                        for (b, p) in macro_params.into_iter() {
                            let t = t_iter.next().expect("macro check failed");
                            let v = if b {
                                Quote(vec![t])
                            } else {
                                t.eval(ctxt)?
                            };
                            next.put(p, v, false, &None)?;
                        }
                        if let Some(r) = op_rest {
                            next.put(r.clone(), Quote(t_iter.collect()), false, &None)?;
                        }
                        body.as_ref().eval(&next)
                    },
                    Err((vs, file_pos)) => 
                        run_fn(ctxt, into_tokens(vs, file_pos), fn_name, 
                            &macro_params.into_iter().map(|(_, i)| i).collect(), op_rest, body),
                }
            },
            Fn(params, op_rest, body) => {
                let tail = match r_tail {
                    Ok(ts) => ts,
                    Err((vs, file_pos)) => into_tokens(vs, file_pos), 
                };
                run_fn(ctxt, tail, fn_name, params, op_rest, body)
            },
            BuiltIn(bifn) => {
                let tail = match r_tail {
                    Ok(ts) => ts,
                    Err((vs, file_pos)) => into_tokens(vs, file_pos),
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
