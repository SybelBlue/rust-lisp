use std::{fmt::{Display, Formatter}, collections::{HashSet, HashMap}};

use super::{subst::{Substitutable, Subst}, Type};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scheme {
    pub(crate) forall: Vec<usize>,
    pub(crate) tipe: Type,
}

impl Display for Scheme {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.tipe.fmt(f)
    }
}

impl Scheme {
    pub(crate) fn concrete(tipe: Type) -> Self {
        Self { forall: Vec::new(), tipe }
    }

    pub(crate) fn instantiate<F: FnMut() -> Type>(&self, f: &mut F) -> Type {
        let Self { forall, tipe } = self;
        let sub = Subst::from(
            forall.iter().map(|o| (*o, f())).collect()
        );
        tipe.apply(&sub)
    }

    pub(crate) fn normalize(&self) -> Scheme {
        let mut mapping = HashMap::new();
        let tipe = self.tipe.normalize(&mut mapping);
        Self { 
            forall: mapping.values().map(|x| *x).collect(), 
            tipe
        }
    }
}

impl Substitutable for Scheme {
    fn apply(&self, sub: &Subst) -> Self {
        Self { 
            forall: self.forall.clone(),
            tipe: self.tipe.apply(&sub.delete_all(&self.forall)),
        }
    }

    fn ftv(&self, used: &mut HashSet<usize>) {
        self.tipe.ftv(used);
        self.forall.iter().for_each(|v| { used.remove(v); });
    }
}