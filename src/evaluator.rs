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
            Error::ParseError(p) => write!(f, "{}", p),
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
    Fn(Vec<Ident>, Box<Token>),
    BuiltIn(String, fn(Vec<Value>) -> EvalResult<Value>),
}

impl Value {
    pub fn eval(&self, ctxt: &Context, tail: Vec<Token>, fn_name: Option<&String>) -> EvalResult<Value> {
        match self {
            Value::Fn(params, body) => {
                if tail.len() != params.len() {
                    let f_name = if let Some(s) = fn_name { s.clone() } else { format!("<anon func>") };
                    return Err(Error::ArgError { f_name, expected: params.len(), recieved: tail.len() })
                }
                let mut args = Vec::new();
                for t in tail {
                    args.push(t.eval(ctxt)?);
                }
                let data = params.into_iter()
                    .zip(args)
                    .map(|(k, v)| (k.name.clone(), (v, Some(k.file_pos))))
                    .collect();
                let next = ctxt.chain(data);
                body.as_ref().eval(&next)
            },
            Value::BuiltIn(_, f) => {
                let mut args = Vec::new();
                for t in tail {
                    args.push(t.eval(ctxt)?);
                }
                f(args)
            },
            v => if tail.is_empty() { 
                Ok(v.clone()) 
            } else { 
                Err(Error::ValueError(v.clone(), format!("not a function"))) 
            },
        }
    }
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

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::Fn(_, x), Value::Fn(_, y)) => x.expr == y.expr,
            (Value::BuiltIn(_, x), Value::BuiltIn(_, y)) => x == y,
            _ => false,
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

impl Expr {
    pub fn eval(&self, ctxt: &Context, file_pos: FilePos) -> EvalResult<Value> {
        match self {
            Expr::Lit(v) => Ok(v.clone()),
            Expr::Var(id) => ctxt.get(&Ident::new(id.clone(), file_pos)),
            Expr::Form(tks) => {
                if let Some((head, tail)) = tks.split_first() {
                    head.eval(ctxt)?.eval(ctxt, Vec::from(tail), head.expr.get_var_name())
                } else {
                    Ok(Value::Unit)
                }
            },
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
        if let Expr::Var(s) = self {
            Some(s)
        } else {
            None
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
