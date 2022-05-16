use crate::exprs::{Ident, Expr};

pub(crate) type Constructor<'a> = (Ident<'a>, Expr<'a>);

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
