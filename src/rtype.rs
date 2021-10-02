use crate::value::*;

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
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Int => write!(f, "Int"),
            Self::TVar(v) => write!(f, "{}", v),
            Self::Data(ident, ts) => {
                if ts.is_empty() {
                    println!("here!");
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
