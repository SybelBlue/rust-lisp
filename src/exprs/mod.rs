use std::fmt::{Display, Formatter, Result, Write};

pub mod types;
pub mod values;
pub mod contexts;

use crate::parsing::FilePos;

use self::values::Value;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct SBody<'a, T> 
    where T: PartialEq + Eq + PartialOrd + Ord {
    pub start: FilePos<'a>,
    pub body: Vec<T>,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expr<'a> {
    Val(Value<'a>),
    SExp(SBody<'a, Expr<'a>>),
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Val(v) => write!(f, "{}", v),
            Expr::SExp(SBody { body, .. }) => {
                f.write_char('(')?;
                if let Some((fst, rst)) = body.split_first() {
                    write!(f, "{}", fst)?;
                    for e in rst {
                        write!(f, " {}", e)?;
                    }
                }
                f.write_char(')')
            },
        }
    }
}