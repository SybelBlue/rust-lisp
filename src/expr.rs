use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Val(Value),
    Var(String),
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
}