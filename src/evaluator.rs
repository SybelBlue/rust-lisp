use std::collections::HashMap;

use crate::{context::Context, parser::{ParseError, ParseStream, parse_all}};

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

impl std::fmt::Display for FilePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

pub type EvalResult<T> = Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    ValueError(Value, String),
    ArgError { f_name: String, recieved: usize, expected: usize },
    IllegalDefError(Ident),
    NameError(Ident),
    RedefError(Ident, String),
    ParseError(ParseError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ValueError(v, s) => write!(f, "ValueError({}): {}", v, s),
            Error::ArgError { f_name, recieved, expected } => 
                write!(f, "ArgError({}): recieved {} arguments, expected {}", f_name, recieved, expected),
            Error::IllegalDefError(n) => 
                write!(f, "IllegalDefError({}): cannot define in immutable scope at {}", n.name, n.file_pos),
            Error::NameError(n) => write!(f, "NameError({}): not defined in scope at {}", n.name, n.file_pos),
            Error::RedefError(orig, n) => 
                write!(f, "RedefError({}): cannot redefine {} at {}", n, orig.name, orig.file_pos),
            Error::ParseError(p) => write!(f, "ParseError: {}", p),
        }
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
    Fn(Vec<Ident>, Option<Ident>, Box<Token>),
    BuiltIn(BuiltInFn),
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

fn form_string(form: &Vec<Token>) -> String {
    form.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(" ")
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
