use std::fmt::{Display, Formatter, Result, Write};

pub mod types;
pub mod values;
pub mod contexts;

use crate::parsing::FilePos;

use self::values::Value;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expr<'a> {
    Val(Value<'a>),
    SExp(FilePos<'a>, Vec<Expr<'a>>),
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Val(v) => write!(f, "{}", v),
            Expr::SExp(_, es) => {
                f.write_char('(')?;
                if !es.is_empty() {
                    write!(f, "{}", es[0])?;
                    for e in &es[1..] {
                        write!(f, " {}", e)?;
                    }
                }
                f.write_char(')')
            },
        }
    }
}