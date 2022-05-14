use std::collections::HashSet;

use crate::{
    errors::{TypeResult, TypeError, TypeErrorBody::*}, 
    parsing::sources::Loc, 
    typing::{Type, subst::{Subst, Substitutable, occurs_check}}
};

type Constr = (Type, Type);
pub(crate) type Constraint<'a> = Loc<'a, Constr>;

type SubstResult<'a> = TypeResult<'a, Subst>;

impl<'a> Substitutable for Constraint<'a> {
    fn apply(&self, sub: &Subst) -> Self {
        let (l , r) = &self.body;
        Self { 
            pos: self.pos.clone(), 
            body: (l.apply(sub), r.apply(sub)) 
        }
    }

    fn ftv(&self, used: &mut HashSet<usize>) {
        self.body.0.ftv(used);
        let mut other = HashSet::new();
        self.body.1.ftv(&mut other);
        other.iter().for_each(|v| { used.remove(v); });
    }
}

pub(crate) fn solve(cs: Vec<Constraint>) -> SubstResult {
    // println!("solving!");
    cs.into_iter().try_fold(
        Subst::empty(), 
        |sub, c| {
            // println!("unifying {:?} -> ", c.body);
            let ref su2 = unify(c.apply(&sub))?;
            // println!("\t{:?}", su2);
            Ok(su2.compose(&sub))
        }
    )
}

fn unify(Constraint { pos, body }: Constraint) -> SubstResult {
    use Type::*;
    match body {
        (t1, t2) if t1 == t2 => 
            Ok(Subst::empty()),
        (Var(v), t) | (t, Var(v)) => {
            if t == Var(v) {
                Ok(Subst::empty())
            } else if occurs_check(&v, &t) {
                Err(TypeError::new(pos, InfiniteType(Var(v), t)))
            } else {
                Ok(Subst::singleton(v, t)) 
            }
        }
        (Fun(p1, r1), Fun(p2, r2)) => {
            let ref su1 = unify(
                Constraint { pos: pos.clone(), body: (*p1, *p2) }
            )?;

            let su2 = unify(
                Constraint { pos, body: (r1.apply(su1), r2.apply(su1)) }
            )?;

            Ok(su2.compose(su1))
        }
        (got, expected) =>
            Err(TypeError::new(pos, TypeMismatch { got, expected }))
    }
}
