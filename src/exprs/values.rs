use crate::errors::Loc;

use super::{Expr, typing::Type};

pub type VToken<'a> = Loc<'a, Value<'a>>;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nat(usize),
    Sym(String),
    Lam(Box<Expr<'a>>, Box<Expr<'a>>),
    Type(Type)
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nat(n) => write!(f, "{}", n),
            Value::Sym(s) => write!(f, "{}", s),
            Value::Lam(p, b) => write!(f, "(\\{} {})", p, b),
            Value::Type(t) => write!(f, "{}", t),
        }
    }
}