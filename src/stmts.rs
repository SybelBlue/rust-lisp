use std::fmt::{Display, Formatter, Result};

use crate::{parsing::sources::FilePos, exprs::{Expr, Ident, SToken}, values::{Value, VToken}};



#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expr(Expr<'a>),
    Bind(Ident<'a>, Expr<'a>),
}

impl<'a> Stmt<'a> {
    pub(crate) fn value(pos: FilePos<'a>, v: Value<'a>) -> Self {
        Self::Expr(Expr::Val(VToken { pos, body: v }))
    }
    
    pub(crate) fn sexp(pos: FilePos<'a>, v: Vec<Expr<'a>>) -> Self {
        Self::Expr(Expr::SExp(SToken { pos, body: v }))
    }

    pub(crate) fn free_symbols(&self) -> Vec<&String> {
        match self {
            Stmt::Expr(e) => e.free_symbols(),
            Stmt::Bind(name, l) => {
                let mut out = Vec::new();
                for s in l.free_symbols() {
                    if s != &name.body {
                        out.push(s);
                    }
                }
                out
            }
        }
    }
}

impl<'a> Display for Stmt<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Expr(e) => e.fmt(f),
            Self::Bind(lstr, _) => lstr.display_simple(f),
        }
    }
}