use std::{fmt::{Display, Formatter, Write, Debug}, collections::HashMap};

use crate::{exprs::Ident, parsing::{sources::{Loc, FilePos}, try_collect}};

pub type Pattern<'a> = Loc<'a, PatternBody<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternBody<'a> {
    PSym(String),
    PSExp(Ident<'a>, Vec<Pattern<'a>>),
}

impl<'a> Pattern<'a> {
    pub(crate) fn dedup_idents(self) -> Result<Self, (String, FilePos<'a>, FilePos<'a>)> {
        self.dedup_(&mut HashMap::new())
    }

    pub(crate) fn first_ident(&self) -> (&FilePos, &String) {
        use PatternBody::*;
        match &self.body {
            PSym(w) => 
                (&self.pos, w),
            PSExp(fst, _) =>
                (&fst.pos, &fst.body),
        }
    }

    fn dedup_(self, used: &mut HashMap<String, FilePos<'a>>) -> Result<Self, (String, FilePos<'a>, FilePos<'a>)> {
        match self.body {
            PatternBody::PSym(w) => {
                if let Some(old) = used.insert(w.clone(), self.pos.clone()) {
                    Err((w, old.clone(), self.pos))
                } else {
                    Ok(Self{ pos: self.pos, body: PatternBody::PSym(w) })
                }
            }
            PatternBody::PSExp(fst, s) => {
                if let Some(old) = used.get(&fst.body) {
                    Err((fst.body, old.clone(), self.pos))
                } else {
                    Ok(Self {
                        pos: self.pos,
                        body: PatternBody::PSExp(
                            fst,
                            try_collect(s.into_iter().map(|p| p.dedup_(used)))?
                        )
                    })
                }
            }
        }
    }
}

impl<'a> Display for PatternBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PSym(s) => 
                f.write_str(s),
            Self::PSExp(fst, ps) => {
                write!(f, "({fst}")?;
                for p in ps {
                    write!(f, " {p}")?;
                }
                f.write_char(')')
            }
        }
    }
}

pub(crate) type Constructor<'a> = (Pattern<'a>, Pattern<'a>);

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
