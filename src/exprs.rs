use std::{fmt::{Display, Formatter, Result, Write}};

use crate::{parsing::sources::Loc, values::VToken};

pub type Ident<'a> = Loc<'a, String>;

pub type SToken<'a> = Loc<'a, Vec<Expr<'a>>>;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Val(VToken<'a>),
    SExp(SToken<'a>),
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Val(l) => l.display_simple(f),
            Self::SExp(l) => {
                if let Some((fst, rst)) = l.body.split_first() {
                    write!(f, "({}", fst)?;
                    rst.into_iter().try_for_each(|e| write!(f, " {}", e))?;
                    f.write_char(')')
                } else {
                    f.write_str("()")
                }
            }
        }
    }
}
