use crate::errors::TypeResult;

use super::{subst::Subst, Type};



pub(crate) type Constraint = (Type, Type);

pub(crate) fn solve<'a>(cs: Vec<Constraint>) -> TypeResult<'a, Subst> {
    todo!()
}