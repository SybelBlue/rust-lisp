use std::{fmt::{Display, Formatter, Result, Write}};

pub mod values;

use crate::errors::Loc;

use self::values::{Value, VToken};

pub type SToken<'a, T> = Loc<'a, SExp<T>>;

#[derive(Debug, Clone)]
pub struct SExp<T>(pub Vec<T>);

impl<T> Display for SExp<T> 
    where T: Display {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some((fst, rst)) = self.0.split_first() {
            write!(f, "({}", fst)?;
            rst.into_iter().try_for_each(|e| write!(f, " {}", e))?;
            f.write_char(')')
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Val(VToken<'a>),
    SExp(SToken<'a, Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub(crate) fn get_symbols(&'a self) -> Vec<String> {
        let mut symbols = Vec::new();
        let mut to_check = vec![self];
        while let Some(next) = to_check.pop() {
            match next {
                Expr::Val(VToken { body: Value::Sym(w), .. }) => symbols.push(w.clone()),
                Expr::Val(_) => {},
                Expr::SExp(sbody) => to_check.extend(&sbody.body.0),
            }
        }
        symbols
    }

    pub(crate) fn display_simple(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Val(l) => l.display_simple(f),
            Self::SExp(l) => l.display_simple(f),
        }
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.display_simple(f)
    }
}