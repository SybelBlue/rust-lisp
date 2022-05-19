use std::fmt::{Display, Formatter, Result};

use crate::{parsing::sources::FilePos, exprs::{Expr, Ident, ExprBody}, values::Value, data::DataDecl};

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Bind(Ident<'a>, Expr<'a>),
    Decl(DataDecl<'a>),
}

impl<'a> Stmt<'a> {
    pub(crate) fn value(pos: FilePos<'a>, v: Value<'a>) -> Self {
        Self::Expr(Expr { pos, body: ExprBody::Val(v) })
    }
    
    pub(crate) fn sexp(pos: FilePos<'a>, v: Vec<Expr<'a>>) -> Self {
        Self::Expr(Expr{ pos, body: ExprBody::SExp(v) })
    }
}

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Expr(e) => 
                e.body.fmt(f),
            Self::Bind(lstr, _) => 
                f.write_str(&lstr.body),
            Self::Decl(decl) => 
                f.write_str(&decl.name.body),
        }
    }
}