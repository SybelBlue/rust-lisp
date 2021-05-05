use crate::{context::Context, evaluator::{Expr, result::{FilePos, EvalResult}, value::{Value, Ident}}};

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

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool, namespace: &Option<String>) -> EvalResult<Value> {
        self.expr.exec(ctxt, allow_overwrite, self.file_pos, namespace)
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