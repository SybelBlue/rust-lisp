use crate::{
    context::Context, 
    parser::{ParseStream, parse_all}, 
    evaluator::{token::Token, value::*, result::*}
};

pub mod value;
pub mod result;
pub mod token;

pub fn exec(s: String) -> (Vec<EvalResult<Value>>, Context<'static>) {
    let mut ctxt = Context::new();
    let out = exec_using(s, &mut ctxt, &None);
    (out, ctxt)
}

pub fn exec_using(s: String, ctxt: &mut Context, namespace: &Option<String>) -> Vec<EvalResult<Value>> {
    let op_exprs = parse_all(&mut ParseStream::new(&mut s.chars().peekable()));
    let mut out = Vec::new();
    for e in op_exprs {
        match e {
            Err(s) => out.push(Err(Error::ParseError(s))),
            Ok(x) => out.push(x.exec(ctxt, false, namespace))
        }
    }
    out
}

pub fn eval_all(ctxt: &Context<'_>, tokens: Vec<Token>) -> EvalResult<Vec<Value>> {
    let mut out = Vec::new();
    for e in tokens {
        out.push(e.eval(ctxt)?);
    }
    Ok(out)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Var(String),
    Form(Vec<Token>),
    Def(Ident, Box<Token>),
    Import(Ident, Option<Ident>),
}

fn run_form(form: &Vec<Token>, ctxt: &Context) -> EvalResult<Value> {
    if let Some((head, tail)) = form.split_first() {
        head.eval(ctxt)?.eval(ctxt, Ok(Vec::from(tail)), &head.expr.get_var_name())
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
            Expr::Import(n, _) => Err(Error::IllegalDefError(n.clone())),
        }
    }

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool, file_pos: FilePos, namespace: &Option<String>) -> EvalResult<Value> {
        match self {
            Expr::Def(n, body) => {
                ctxt.put(n.clone(), body.as_ref().eval(&ctxt)?, allow_overwrite, namespace)?;
                Ok(Value::Unit)
            },
            Expr::Import(n, namespace) => {
                let dir = std::env::current_dir()
                    .map_err(|e| Error::ImportError(n.clone(), e.to_string()))?;
                let path = std::env::join_paths(vec![dir.as_path(), std::path::Path::new(n.name.as_str())])
                    .map_err(|e| Error::ImportError(n.clone(), e.to_string()))?;
                let s = std::fs::read_to_string(path)
                    .map_err(|e| Error::ImportError(n.clone(), e.to_string()))?;
                
                exec_using(s, ctxt, &namespace.clone().map(|id| id.name));

                Ok(Value::Unit)
            },
            _ => self.eval(&ctxt, file_pos),
        }
    }

    pub fn get_var_name(&self) -> Option<String> {
        match self {
            Expr::Var(n) => Some(n.clone()),
            Expr::Lit(Value::BuiltIn(bifn)) => Some(bifn.name.clone()),
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
            Expr::Import(n, Some(b)) => write!(f, "(import {} {})", n, b),
            Expr::Import(n, None) => write!(f, "(import {})", n),
        }
    }
}
