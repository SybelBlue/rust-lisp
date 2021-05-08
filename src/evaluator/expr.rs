use crate::evaluator::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Var(String),
    Form(Vec<Token>),
    Def(Ident, Box<Token>),
    Import(Ident, Option<Ident>),
}

pub fn run_form(form: &Vec<Token>, ctxt: &Context) -> EvalResult<Value> {
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
                let mut path = dir.join(n.name.as_str());
                path.set_extension("rsp");
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
