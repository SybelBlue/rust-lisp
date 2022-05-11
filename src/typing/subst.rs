use std::collections::{HashMap, HashSet};

use super::Type;

pub(crate) struct Subst(pub(crate) HashMap<usize, Type>);

impl Subst {
    pub(crate) fn empty() -> Self {
        Self(HashMap::new())
    }

    pub(crate) fn compose(mut self, other: Self) -> Self {
        self.0.extend(other.0.into_iter());
        self
    }

    pub(crate) fn get_default<'a>(&'a self, k: &'a usize, default: &'a Type) -> &'a Type {
        if let Some(t) = self.0.get(k) {
            t
        } else {
            default
        }
    }

    pub(crate) fn delete_all(&self, vars: &Vec<usize>) -> Self {
        let mut out = self.0.clone();
        vars.into_iter().for_each(|v| { out.remove(v); });
        Self(out)
    }
}

pub(crate) trait Substitutable {
    fn apply(&self, sub: &Subst) -> Self;
    fn ftv(&self, used: &mut HashSet<usize>);
}

impl<T: Substitutable> Substitutable for Vec<T> {
    fn apply(&self, sub: &Subst) -> Self {
        self.into_iter().map(|t| t.apply(sub)).collect()
    }

    fn ftv(&self, used: &mut HashSet<usize>) {
        self.into_iter().for_each(|t| t.ftv(used));
    }
}

pub(crate) fn occurs_check<T: Substitutable>(var: &usize, t: T) -> bool {
    let mut used = HashSet::new();
    t.ftv(&mut used);
    used.contains(var)
}