use crate::{context::Context, parser::{ParseError, ParseStream, Token, parse_all}};

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

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Var(String),
    Form(Ident, Vec<Token>),
    Def(Ident, Box<Token>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Fn(Vec<Ident>, Box<Token>),
    BuiltIn(String, fn(Vec<Value>) -> EvalResult<Value>),
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
    pub fn eval(&self, ctxt: &Context, file_pos: FilePos) -> EvalResult<Value> {
        match self {
            Expr::Lit(v) => Ok(v.clone()),
            Expr::Var(id) => ctxt.get(&Ident { name: id.clone(), file_pos }),
            Expr::Form(h, tail) => {
                match ctxt.get(&h)? {
                    Value::Fn(params, body) => {
                        if tail.len() != params.len() {
                            return Err(Error::ArgError { f_name: h.name.clone(), expected: params.len(), recieved: tail.len() })
                        }
                        let mut args = Vec::new();
                        for t in tail {
                            args.push(t.eval(ctxt)?);
                        }
                        let data = params.into_iter()
                            .zip(args)
                            .map(|(k, v)| (k.name, (v, Some(k.file_pos))))
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
                    v => Ok(v),
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
}

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
