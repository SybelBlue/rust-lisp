use std::collections::HashMap;

use super::Type;

pub type Ident = String;
pub type QualifiedIdent = String;


#[derive(Debug, Clone)]
pub struct TypeContext {
    bound: HashMap<QualifiedIdent, Type>,
    aliased: HashMap<Ident, QualifiedIdent>,
    type_vars: HashMap<usize, Option<Type>>,
}

pub enum UnifyErr { Inf, Mis }

impl TypeContext {
    pub fn new() -> Self {
        use super::Type::*;
        type Type_ = super::Type;
        let fun = super::Type::fun;
        
        let mut bound = HashMap::with_capacity(10);
        let mut aliased = HashMap::with_capacity(10);
        
        fn type_binding(t: Type_, bound: &mut HashMap<String, Type_>, aliased: &mut HashMap<String, String>) {
            let al = format!("{}", t);
            let qual = format!("Prelude.{}", al);
            bound.insert(qual.clone(), Type);
            aliased.insert(al, qual);
        }

        bound.insert(format!("Prelude.->"), fun(Type, fun(Type, Type)));
        aliased.insert(format!("->"), format!("Prelude.->"));

        bound.insert(format!("Prelude.+"), fun(Nat, fun(Nat, Nat)));
        aliased.insert(format!("+"), format!("Prelude.+"));
        for t in [Type, Unit, Nat, Char] {
            type_binding(t, &mut bound, &mut aliased);
        }
        
        Self {
            bound,
            aliased,
            type_vars: HashMap::with_capacity(1000),
        }
    }

    pub(crate) fn get(&self, k: &String) -> Option<&Type> {
        self.bound.get(self.aliased.get(k).unwrap_or(k))
    }

    pub(crate) fn query_tvar(&self, var: usize) -> Type {
        let mut curr = var;
        loop {
            match self.type_vars.get(&curr).unwrap() {
                None => return Type::Var(curr),
                Some(Type::Var(next)) => { curr = *next; }
                Some(t) => return t.clone(),
            }
        }
    }

    pub(crate) fn query(&self, t: &Type) -> Type {
        // println!("Query: {:?}", t);
        match t {
            Type::Unit | Type::Nat | Type::Char | Type::Type => t.clone(),
            Type::Data(_, t) => self.query(t.as_ref()),
            Type::Var(v) => self.query_tvar(*v),
            Type::Fun(p, r) => 
                Type::fun(self.query(p), self.query(r)),
        }
    }

    fn put_eq(mut self, var: usize, t: Type) -> Result<Self, UnifyErr> {
        if Some(&None) == self.type_vars.get(&var) {
            self.type_vars.insert(var, Some(t.clone()));
            Ok(self)
        } else {
            panic!("bad eq: var {} ~ {:?}", var, t)
        }
    }

    pub(crate) fn bind_to_tvar(self, name: Ident) -> (Self, usize) {
        let (mut slf, k) = self.new_tvar();
        slf.bound.insert(name, Type::Var(k));
        (slf, k)
    }

    pub(crate) fn new_tvar(mut self) -> (Self, usize) {
        let k = self.type_vars.len();
        self.type_vars.insert(k, None);
        (self, k)
    }

    pub(crate) fn unify(self, expected: &Type, got: &Type) -> Result<Self, UnifyErr> {
        match (self.query(expected), self.query(got)) {
            // if both are functions, unpack and recurse
            (Type::Fun(p0, r0), Type::Fun(p1, r1)) =>
            self.unify(&p0, &p1)
                    .and_then(|new| new.unify(&r0, &r1)),
            // if equal, done.
            (slf, got) if slf == got => Ok(self),
            // if inf, err.
            (slf, got) if slf.contains(&got) || got.contains(&slf) => 
                Err(UnifyErr::Inf),
            // if both are tvars, register greatest equivalence
            (Type::Var(s), Type::Var(o)) =>
                self.put_eq(s.min(o), Type::Var(s.max(o))),
            // if either is a tvar, register an equivalency on the other
            (Type::Var(v), o) 
                | (o, Type::Var(v)) =>
                    self.put_eq(v, o.clone()),
            // unequal, and neither are vars, so not unification possible
            _ => Err(UnifyErr::Mis)
        }
    }
}