use crate::exprs::{Expr, Ident};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nat(usize),
    Sym(String),
    Char(char),
    Lam(Ident<'a>, Box<Expr<'a>>),
}

impl<'a> Value<'a> {
    pub fn lam(p: Ident<'a>, r: Expr<'a>) -> Self {
        Self::Lam(p, Box::new(r))
    }
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "{:?}", c),
            Value::Nat(n) => write!(f, "{}", n),
            Value::Sym(s) => write!(f, "{}", s),
            Value::Lam(p, b) => write!(f, "{} -> {}", p, b),
        }
    }
}