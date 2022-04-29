use std::{fmt::{Display, Formatter, Result, Write}};

use crate::{parsing::sources::Loc, values::{Value, VToken}};

pub type Ident<'a> = Loc<'a, String>;

pub type SToken<'a> = Loc<'a, Vec<Expr<'a>>>;

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Val(VToken<'a>),
    SExp(SToken<'a>),
}

impl<'a> Expr<'a> {
    pub(crate) fn get_lambda_param_names(&'a self) -> Vec<String> {
        let mut symbols = Vec::new();
        let mut to_check = vec![self];
        while let Some(next) = to_check.pop() {
            match next {
                Expr::Val(VToken { body: Value::Sym(w), .. }) => symbols.push(w.clone()),
                Expr::Val(_) => {},
                Expr::SExp(sbody) => to_check.extend(&sbody.body),
            }
        }
        symbols
    }

    pub(crate) fn free_symbols(&self) -> Vec<&String> {
        match self {
            Expr::Val(v) => v.body.free_symbols(),
            Expr::SExp(s) => s.body.iter().flat_map(|e| e.free_symbols()).collect(),
        }
    }
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
                    Ok(())
                }
            }
        }
    }
}
