use crate::{parsing::sources::Loc, exprs::Expr};

pub type VToken<'a> = Loc<'a, Value<'a>>;

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Nat(usize),
    Sym(String),
    Char(char),
    Lam(Box<Expr<'a>>, Box<Expr<'a>>),
}

impl<'a> Value<'a> {
    pub fn lam(p: Expr<'a>, r: Expr<'a>) -> Self {
        Self::Lam(Box::new(p), Box::new(r))
    }

    pub(crate) fn free_symbols(&self) -> Vec<&String> {
        match self {
            Value::Nat(_) | Value::Char(_) => vec![],
            Value::Sym(s) => vec![s],
            Value::Lam(p, r) => {
                let params = p.free_symbols();
                r.free_symbols().into_iter().filter(|s| !params.contains(s)).collect()
            }
        }
    }
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Char(c) => write!(f, "'{}'", c),
            Value::Nat(n) => write!(f, "{}", n),
            Value::Sym(s) => write!(f, "{}", s),
            Value::Lam(p, b) => write!(f, "{} -> {}", p, b),
        }
    }
}