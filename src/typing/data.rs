use crate::exprs::Ident;

use super::scheme::Scheme;

pub(crate) type Constructor<'a> = (Ident<'a>, Scheme);

#[derive(Debug, Clone)]
pub(crate) struct DataDecl<'a> {
    name: Ident<'a>, 
    kind: Kind, 
    ctors: Vec<Constructor<'a>>,
}

#[derive(Debug, Clone)]
pub enum Kind {
    Type,
    KFn(Box<Kind>, Box<Kind>),
}
