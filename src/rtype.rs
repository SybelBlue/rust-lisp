use crate::{
    context::Context,
    expr::Expr,
    result::{Error, EvalResult},
    value::*,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Data(Ident, Vec<Type>), // just the name and type vars
    TVar(String),
    Arrow(Box<Type>, Box<Type>),
    Constraint(Ident, Vec<Type>, Box<Type>),
}

impl Type {
    pub fn from_ident(ident: Ident) -> Self {
        let name_bytes = ident.name.as_bytes();
        if name_bytes == b"Int" {
            Type::Int
        } else if name_bytes[0].is_ascii_uppercase() {
            Self::Data(ident, vec![])
        } else {
            Self::TVar(ident.name)
        }
    }

    pub fn depth(&self) -> usize {
        if let Type::Arrow(_, t) = self {
            1 + t.depth()
        } else {
            0
        }
    }
}

pub fn reify<'a>(e: &'a Expr, tctxt: &'a Context<'a>) -> EvalResult<&'a Type> {
    match e {
        Expr::Val(v) => Ok(v.get_type()),
        Expr::Var(v) => tctxt.get_type(v),
        Expr::Form(v) if v.len() == 1 => reify(&v[0], tctxt),
        Expr::Form(v) => {
            let mut es = v.iter();
            if let Some(head) = es.next() {
                let head_type = reify(head, tctxt)?;
                Err(Error::InternalError(format!(
                    "todo: {} {:?}",
                    head_type,
                    es.collect::<Vec<&Expr>>()
                )))
            } else {
                Ok(&Type::Unit)
            }
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Int => write!(f, "Int"),
            Self::TVar(v) => write!(f, "{}", v),
            Self::Data(ident, ts) => {
                if ts.is_empty() {
                    return write!(f, "{}", ident.name);
                }
                write!(f, "({}", ident.name)?;
                for t in ts {
                    write!(f, " {}", t)?;
                }
                write!(f, ")")
            }
            Self::Arrow(t0, t1) => write!(f, "(-> {} {})", t0, t1),
            Self::Constraint(ident, ts, body) => {
                if ts.is_empty() {
                    return write!(f, "(=> {} {})", ident.name, body);
                }
                write!(f, "(=> ({}", ident.name)?;
                for t in ts {
                    write!(f, " {}", t)?;
                }
                write!(f, ") {})", body)
            }
        }
    }
}
