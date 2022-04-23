use std::{fmt::{Display, Formatter, Result}};

pub mod types;
pub mod values;
pub mod contexts;

use crate::parsing::sources::FilePos;

use self::values::Value;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct SBody<'a, T> 
    where T: PartialEq + Eq + PartialOrd + Ord {
    pub start: FilePos<'a>,
    pub body: Vec<T>,
}

impl<'a, T> Display for SBody<'a, T> 
    where T: Display + PartialEq + Eq + PartialOrd + Ord + Clone {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some((fst, rst)) = self.body.split_first() {
            write!(f, "{}", fst)?;
            rst.into_iter().try_for_each(|e| write!(f, " {}", e))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expr<'a> {
    Val(Value<'a>),
    SExp(SBody<'a, Expr<'a>>),
}

impl<'a> Expr<'a> {
    pub fn get_symbols(&'a self) -> Vec<String> {
        let mut symbols = Vec::new();
        let mut to_check = vec![self];
        while let Some(next) = to_check.pop() {
            match next {
                Expr::Val(Value::Sym(w)) => symbols.push(w.clone()),
                Expr::Val(_) => {},
                Expr::SExp(sbody) => to_check.extend(&sbody.body),
            }
        }
        symbols
    }
}

impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Expr::Val(v) => write!(f, "{}", v),
            Expr::SExp(sbody) => write!(f, "({})", sbody),
        }
    }
}