use std::{fmt::{Display, Formatter, Write, Debug}, collections::HashMap};

use crate::{exprs::Ident, parsing::{sources::{Loc, FilePos}, try_collect}};

pub type Data<'a> = Loc<'a, DataBody<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataBody<'a> {
    PSym(String),
    PSExp(Ident<'a>, Vec<Data<'a>>),
}

impl<'a> Data<'a> {
    pub(crate) fn dedup_idents(self) -> Result<Self, (String, FilePos<'a>, FilePos<'a>)> {
        self.dedup_(&mut HashMap::new())
    }

    pub(crate) fn split_first(&'a self) -> (&'a FilePos<'a>, &'a String, Option<&'a Vec<Data<'a>>>) {
        use DataBody::*;
        match &self.body {
            PSym(w) => 
                (&self.pos, w, None),
            PSExp(fst, rst) =>
                (&fst.pos, &fst.body, Some(rst)),
        }
    }

    fn dedup_(self, used: &mut HashMap<String, FilePos<'a>>) -> Result<Self, (String, FilePos<'a>, FilePos<'a>)> {
        match self.body {
            DataBody::PSym(w) => {
                if let Some(old) = used.insert(w.clone(), self.pos.clone()) {
                    Err((w, old.clone(), self.pos))
                } else {
                    Ok(Self{ pos: self.pos, body: DataBody::PSym(w) })
                }
            }
            DataBody::PSExp(fst, s) => {
                if let Some(old) = used.get(&fst.body) {
                    Err((fst.body, old.clone(), self.pos))
                } else {
                    Ok(Self {
                        pos: self.pos,
                        body: DataBody::PSExp(
                            fst,
                            try_collect(s.into_iter().map(|p| p.dedup_(used)))?
                        )
                    })
                }
            }
        }
    }
}

impl<'a> Display for DataBody<'a> {
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

pub(crate) type Constructor<'a> = (Data<'a>, Data<'a>);

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

impl Kind {
    pub fn kfun(p: Kind, b: Kind) -> Kind {
        Kind::KFun(Box::new(p), Box::new(b))
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Type => 
                f.write_str("Type"),
            Kind::KFun(p, r) => 
                write!(f, "(-> {p} {r})"),
        }
    }
}
