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

impl std::fmt::Display for FilePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

pub type ParseError = String;
pub type ParseResult<T> = Result<T, ParseError>;

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
    Idnt(Ident),
    Form(Ident, Vec<Expr>),
    Def(Ident, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Fn(Vec<String>, Box<Expr>),
    BuiltIn(String, fn(Vec<Value>) -> ParseResult<Value>),
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
    pub fn eval(&self, ctxt: &Context) -> ParseResult<Value> {
        match self {
            Expr::Lit(v) => Ok(v.clone()),
            Expr::Idnt(id) => ctxt.get(&id.name),
            Expr::Form(h, tail) => {
                match ctxt.get(&h.name)? {
                    Value::Fn(params, body) => {
                        if tail.len() != params.len() {
                            return Err(format!(
                                "ArgError: Not enough args provided to {}: expected {}, got {}", 
                                    h.name, params.len(), tail.len()))
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

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool) -> ParseResult<Value> {
        match self {
            Expr::Def(n, body) => {
                ctxt.put(n.name.clone(), body.as_ref().eval(&ctxt)?, allow_overwrite)?;
                Ok(Value::Unit)
            },
            _ => self.eval(&ctxt),
        }
    }
}

pub fn exec(s: String) -> (Vec<ParseResult<Value>>, Context<'static>) {
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
