use std::collections::{VecDeque, HashSet};

use crate::errors::TypeResult;

use super::{subst::{Subst, Substitutable}, Type};

type Unifier = (Subst, VecDeque<Constraint>);

pub(crate) type Constraint = (Type, Type);

type SubstResult<'a> = TypeResult<'a, Subst>;

impl Substitutable for Constraint {
    fn apply(&self, sub: &Subst) -> Self {
        let (t1, t2) = self;
        (t1.apply(sub), t2.apply(sub))
    }

    fn ftv(&self, used: &mut HashSet<usize>) {
        self.0.ftv(used);
        let mut other = HashSet::new();
        self.1.ftv(&mut other);
        other.iter().for_each(|v| { used.remove(v); });
    }
}

pub(crate) fn solve<'a>(cs: Vec<Constraint>) -> SubstResult<'a> {
    solver((Subst::empty(), VecDeque::from(cs)))
}

fn solver<'a>((s, mut cs): Unifier) -> SubstResult<'a> {
    if let Some((t1, t2)) = cs.pop_front() {
        let s2 = unifies(t1, t2)?;
        let new_cs = Substitutable::apply(&cs, &s2);
        solver((s.compose(s2), new_cs))
    } else {
        Ok(s)
    }
}

fn unifies<'a>(t1: Type, t2: Type) -> SubstResult<'a> {
    use Type::*;
    match (t1, t2) {
        (t1, t2) if t1 == t2 => 
            Ok(Subst::empty()),
        (Var(v), t) | (t, Var(v)) =>
            bind(v, t),
        (Type::Fun(t1, t2), Type::Fun(t3, t4)) =>
            unifyMany(vec![*t1, *t2], vec![*t3, *t4]),
        (t1, t2) =>
            todo!("unification fail")
    }
}

fn unifyMany<'a>(ls: Vec<Type>, rs: Vec<Type>) -> SubstResult<'a> {
    todo!()
}

fn bind<'a>(v: usize, t: Type) -> SubstResult<'a> {
    todo!()
}