use crate::{
    context::Context,
    result::EvalResult,
    value::{Ident, Value},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Val(Value),
    Var(Ident),
    Form(Vec<Expr>),
}

impl Expr {
    pub fn depth(&self) -> usize {
        if let Expr::Val(v) = self {
            v.depth()
        } else {
            0
        }
    }

    pub fn eval<'a>(&'a self, ctxt: &'a Context<'a>) -> EvalResult<&'a Value> {
        match self {
            Expr::Val(v) => Ok(v),
            Expr::Var(n) => ctxt.get(n),
            Expr::Form(_) => todo!(),
        }
    }
}
