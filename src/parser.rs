use std::collections::VecDeque;

use crate::{context::Context, lexer::{ParseStream, parse_all}};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePos {
    pub col: usize,
    pub line: usize,
}

impl FilePos {
    pub fn new() -> Self {
        Self { col: 1, line: 1 }
    }

    pub fn advance(&mut self, out: &char) {
        if out == &'\n' || out == &'\r' {
            self.col = 1;
            self.line += 1;
        } else {
            self.col += 1;
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Idnt(String),
    Form(String, Vec<Expr>),
    Def(String, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Fn(VecDeque<String>, Box<Expr>),
    BuiltIn(String, fn(Vec<Value>) -> Result<Value, String>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Int(x) => write!(f, "{}", x),
            Value::Float(x) => write!(f, "{}", x),
            Value::Fn(args, _) => write!(f, "<{}-ary func>", args.len()),
            Value::BuiltIn(s, _) => write!(f, "<builtin func {}>", s),
        }
    }
}

impl Expr {
    pub fn eval(&self, ctxt: &Context) -> Result<Value, String> {
        match self {
            Expr::Lit(v) => Ok(v.clone()),
            Expr::Idnt(id) => ctxt.get(id),
            Expr::Form(h, tail) => {
                match ctxt.get(h)? {
                    Value::Fn(params, body) => {
                        if tail.len() != params.len() {
                            return Err(format!(
                                "ArgError: Not enough args provided to {}: expected {}, got {}", h, params.len(), tail.len()))
                        }
                        let mut args = Vec::new();
                        for a in tail {
                            args.push(a.eval(ctxt)?);
                        }
                        let next = ctxt.chain(params.into_iter().zip(args).collect());
                        body.as_ref().eval(&next)
                    },
                    Value::BuiltIn(_, f) => {
                        let mut args = Vec::new();
                        for a in tail {
                            args.push(a.eval(ctxt)?);
                        }
                        f(args)
                    },
                    v => Ok(v),
                }
            },
            Expr::Def(n, _) => Err(format!("Tried to define in an immutable scope {}", n)),
        }
    }

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool) -> Result<Value, String> {
        match self {
            Expr::Def(n, body) => {
                ctxt.put(n.clone(), body.as_ref().eval(&ctxt)?, allow_overwrite)?;
                Ok(Value::Unit)
            },
            _ => self.eval(&ctxt),
        }
    }
}

pub fn exec(s: String) -> (Vec<Result<Value, String>>, Context<'static>) {
    let mut ctxt = Context::new();
    let op_exprs = parse_all(&mut ParseStream::new(&mut s.chars().peekable()));
    let mut out = Vec::new();
    for e in op_exprs {
        match e {
            Err(s) => out.push(Err(s)),
            Ok(x) => out.push(x.exec(&mut ctxt, false))
        }
    }
    (out, ctxt)
}
