use crate::evaluator::*;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Var(String),
}

pub fn run_form(form: &Vec<Token>, ctxt: &Context) -> EvalResult<Value> {
    unimplemented!()
}

impl Expr {
    pub fn eval(&self, ctxt: &Context, file_pos: FilePos) -> EvalResult<Value> {
        unimplemented!()
    }

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool, file_pos: FilePos, namespace: &Option<String>) -> EvalResult<Value> {
        unimplemented!()
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // match self {
        //     Expr::Lit(x) => write!(f, "{}", x),
        //     Expr::Var(s) => f.write_str(s.as_str()),
        //     Expr::Form(form) => write!(f, "({})", form_string(form)),
        //     Expr::Def(n, b) => write!(f, "(def {} {})", n, b),
        // }
        unimplemented!()
    }
}
