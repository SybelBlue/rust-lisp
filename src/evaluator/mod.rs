pub mod value;
pub mod result;

use crate::{context::Context, parser::{ParseStream, parse_all}};

use self::{value::*, result::*};

pub fn exec(s: String) -> (Vec<EvalResult<Value>>, Context<'static>) {
    let mut ctxt = Context::new();
    let op_exprs = parse_all(&mut ParseStream::new(&mut s.chars().peekable()));
    let mut out = Vec::new();
    for e in op_exprs {
        match e {
            Err(s) => out.push(Err(Error::ParseError(s))),
            Ok(x) => out.push(x.exec(&mut ctxt, false))
        }
    }
    (out, ctxt)
}

pub fn eval_all(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Vec<Value>> {
    let mut out = Vec::new();
    for e in tokens {
        out.push(e.eval(ctxt)?);
    }
    Ok(out)
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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Var(String),
    Form(Vec<Token>),
    Def(Ident, Box<Token>),
}

fn run_form(form: &Vec<Token>, ctxt: &Context) -> EvalResult<Value> {
    if let Some((head, tail)) = form.split_first() {
        head.eval(ctxt)?.eval(ctxt, Vec::from(tail), &head.expr)
    } else {
        Ok(Value::Unit)
    }
}

impl Expr {
    pub fn eval(&self, ctxt: &Context, file_pos: FilePos) -> EvalResult<Value> {
        match self {
            Expr::Lit(v) => Ok(v.clone()),
            Expr::Var(id) => ctxt.get(&Ident::new(id.clone(), file_pos)),
            Expr::Form(form) => run_form(form, ctxt),
            Expr::Def(n, _) => Err(Error::IllegalDefError(n.clone())),
        }
    }

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool, file_pos: FilePos) -> EvalResult<Value> {
        match self {
            Expr::Def(n, body) => {
                ctxt.put(n.clone(), body.as_ref().eval(&ctxt)?, allow_overwrite)?;
                Ok(Value::Unit)
            },
            _ => self.eval(&ctxt, file_pos),
        }
    }

    pub fn get_var_name(&self) -> Option<&String> {
        match self {
            Expr::Var(n) => Some(n),
            Expr::Lit(Value::BuiltIn(bifn)) => Some(&bifn.name),
            _ => None,
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Lit(x) => write!(f, "{}", x),
            Expr::Var(s) => f.write_str(s.as_str()),
            Expr::Form(form) => write!(f, "({})", form_string(form)),
            Expr::Def(n, b) => write!(f, "(def {} {})", n, b),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub expr: Expr,
    pub file_pos: FilePos,
}

impl Token {
    pub fn new(expr: Expr, file_pos: FilePos) -> Self {
        Self { expr, file_pos }
    }

    pub fn from_value(v: Value, file_pos: FilePos) -> Self {
        Self::new(Expr::Lit(v), file_pos)
    }

    pub fn from_ident(ident: Ident) -> Self {
        Self::new(Expr::Var(ident.name), ident.file_pos)
    }

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool) -> EvalResult<Value> {
        self.expr.exec(ctxt, allow_overwrite, self.file_pos)
    }

    pub fn eval(&self, ctxt: &Context) -> EvalResult<Value> {
        self.expr.eval(ctxt, self.file_pos)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expr)
    }
}
