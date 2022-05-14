use std::collections::{HashMap, HashSet};

use super::Type;

#[derive(Debug, Clone)]
pub(crate) struct Subst(HashMap<usize, Type>);

impl Subst {
    pub(crate) fn from(sub: HashMap<usize, Type>) -> Self {
        Self(sub)
    }
    
    pub(crate) fn empty() -> Self {
        Self(HashMap::new())
    }

    pub(crate) fn singleton(var: usize, t: Type) -> Self {
        Self(vec![(var, t)].into_iter().collect())
    }

    pub(crate) fn compose(&self, other: &Self) -> Self {
        let mut s1 = self.0.clone();
        let s2 = &other.0;
        
        let elimmed = 
            s2.iter()
                .map(|(k, v)| (*k, v.apply(self)));
        
        s1.extend(elimmed);

        Subst(s1)
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

pub(crate) fn occurs_check<T: Substitutable>(var: &usize, t: &T) -> bool {
    let mut used = HashSet::new();
    t.ftv(&mut used);
    used.contains(var)
}