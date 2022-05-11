use std::collections::{VecDeque, HashSet};

use crate::{errors::{TypeResult, TypeError, TypeErrorBody}, parsing::sources::{Loc, FilePos}};

use super::{subst::{Subst, Substitutable, occurs_check}, Type};

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

pub(crate) fn solve(cs: Vec<Constraint>) -> SubstResult {
    solver((Subst::empty(), VecDeque::from(cs)))
}

fn solver((s, mut cs): Unifier) -> SubstResult {
    if let Some(c) = cs.pop_front() {
        let s2 = unifies(c)?;
        let new_cs = Substitutable::apply(&cs, &s2);
        solver((s.compose(&s2), new_cs))
    } else {
        Ok(s)
    }
}

fn unifies(c: Constraint) -> SubstResult {
    let Constraint { pos, body: (t1, t2) } = c;
    use Type::*;
    match (t1, t2) {
        (t1, t2) if t1 == t2 => 
            Ok(Subst::empty()),
        (Var(v), t) | (t, Var(v)) =>
            bind(pos, v, t),
        (Type::Fun(t1, t2), Type::Fun(t3, t4)) =>
            unify_many(pos, VecDeque::from(vec![*t1, *t2]), VecDeque::from(vec![*t3, *t4])),
        (t1, t2) =>
            Err(TypeError::new(pos, TypeErrorBody::TypeMismatch { got: t1, expected: t2 }))
    }
}

fn unify_many(pos: FilePos, mut ls: VecDeque<Type>, mut rs: VecDeque<Type>) -> SubstResult {
    if ls.len() != rs.len() { 
        panic!("unification mismatch");
    } else if ls.is_empty() {
        return Ok(Subst::empty());
    }

    let t1 = ls.pop_front().unwrap();
    let t2 = rs.pop_front().unwrap();

    let c = Constraint {
        pos: pos.clone(),
        body: (t1, t2)
    };
    
    let su1 = unifies(c)?;
    let su2 = unify_many(pos.clone(), ls, rs)?;

    Ok(su2.compose(&su1))
}

fn bind(pos: FilePos, var: usize, t: Type) -> SubstResult {
    if t == Type::Var(var) {
        Ok(Subst::empty())
    } else if occurs_check(&var, &t) {
        Err(TypeError::new(pos, TypeErrorBody::InfiniteType(Type::Var(var), t)))
    } else {
        Ok(Subst::singleton(var, t)) 
    }
}