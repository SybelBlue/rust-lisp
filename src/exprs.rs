use std::{fmt::{Display, Formatter, Result, Write}};

use crate::{parsing::sources::{Loc, FilePos}, values::Value};

pub type Ident<'a> = Loc<'a, String>;

pub type Expr<'a> = Loc<'a, ExprBody<'a>>;

#[derive(Debug, Clone)]
pub enum ExprBody<'a> {
    Val(Value<'a>),
    SExp(Vec<Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub(crate) fn val(pos: FilePos<'a>, body: Value<'a>) -> Self {
        Self { pos, body: ExprBody::Val(body) }
    }
}

impl<'a> Display for ExprBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self {
            Self::Val(l) => l.fmt(f),
            Self::SExp(l) => {
                if let Some((fst, rst)) = l.split_first() {
                    write!(f, "({}", &fst.body)?;
                    rst.into_iter().try_for_each(|e| write!(f, " {}", &e.body))?;
                    f.write_char(')')
                } else {
                    f.write_str("()")
                }
            }
        }
    }
}
