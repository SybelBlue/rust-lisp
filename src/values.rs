use crate::{exprs::Expr, data::Data};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nat(usize),
    Sym(String),
    Char(char),
    Lam(Data<'a>, Box<Expr<'a>>),
}

impl<'a> Value<'a> {
    pub fn lam(p: Data<'a>, r: Expr<'a>) -> Self {
        Self::Lam(p, Box::new(r))
    }
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => 
                std::fmt::Debug::fmt(c, f),
            Value::Nat(n) => 
                n.fmt(f),
            Value::Sym(s) => 
                s.fmt(f),
            Value::Lam(p, b) => 
                write!(f, "{} -> {}", p.body, b.body),
        }
    }
}