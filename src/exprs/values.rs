use super::{Expr, types::Type};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Value<'a> {
    Int(usize),
    Sym(String),
    Lam(Box<Expr<'a>>, Box<Expr<'a>>),
    Type(Type)
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Sym(s) => write!(f, "{}", s),
            Value::Lam(p, b) => write!(f, "\\{} {}", p, b),
            Value::Type(t) => write!(f, "{}", t),
        }
    }
}