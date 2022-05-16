use std::fmt::{Display, Formatter, Write};

use crate::exprs::{Ident, Expr};

pub(crate) type Constructor<'a> = (Ident<'a>, Expr<'a>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Pattern<'a> {
    PSym(Ident<'a>),
    PSExp(Vec<Pattern<'a>>),
}

impl<'a> Display for Pattern<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::PSym(s) => 
                f.write_str(&s.body),
            Pattern::PSExp(ps) => {
                f.write_char('(')?;
                if let Some((p, ps)) = ps.split_first() {
                    p.fmt(f)?;
                    for p in ps {
                        write!(f, " {p}")?;
                    }
                }
                f.write_char(')')
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct DataDecl<'a> {
    pub(crate) name: Ident<'a>, 
    pub(crate) kind: Kind, 
    pub(crate) ctors: Vec<Constructor<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Type,
    KFun(Box<Kind>, Box<Kind>),
}

impl Display for Kind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type =>
                f.write_str("Type"),
            Self::KFun(p, r) =>
                write!(f, "(-> {p} {r})"),
        }
    }
}
