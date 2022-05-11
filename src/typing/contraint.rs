use std::collections::{VecDeque, HashSet};

use crate::{errors::TypeResult, parsing::sources::Loc};

use super::{subst::{Subst, Substitutable}, Type};

type Unifier<'a> = (Subst, VecDeque<Constraint<'a>>);

pub(crate) type Constr = (Type, Type);
pub(crate) type Constraint<'a> = Loc<'a, Constr>;

type SubstResult<'a> = TypeResult<'a, Subst>;

impl<'a> Substitutable for Constraint<'a> {
    fn apply(&self, sub: &Subst) -> Self {
        let Self { pos, body: (l , r) } = self;
        Self { pos: pos.clone(), body: (l.apply(sub), r.apply(sub)) }
    }

    fn ftv(&self, used: &mut HashSet<usize>) {
        self.body.0.ftv(used);
        let mut other = HashSet::new();
        self.body.1.ftv(&mut other);
        other.iter().for_each(|v| { used.remove(v); });
    }
}

pub(crate) fn solve<'a>(cs: Vec<Constraint<'_>>) -> SubstResult<'a> {
    solver((Subst::empty(), VecDeque::from(cs)))
}

fn solver<'a>((s, mut cs): Unifier<'_>) -> SubstResult<'a> {
    if let Some(Constraint { body: (l, r), .. }) = cs.pop_front() {
        let s2 = unifies(l, r)?;
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