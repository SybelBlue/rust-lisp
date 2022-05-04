use std::collections::HashMap;

use super::Type;

type Identifier = String;
type QualifiedIdentifier = String;


#[derive(Debug, Clone)]
pub struct Context {
    /// Types are compressed, ie all Var(_n_) inside the type start a 0
    bound: HashMap<QualifiedIdentifier, Type>, 
    aliased: HashMap<Identifier, QualifiedIdentifier>,
}

type VarMap = HashMap<usize, Option<Type>>;

#[derive(Debug, Clone)]
pub(crate) struct Solver {
    /// required on instantiation, released on destruction
    ctxt: Context,
    locals: HashMap<Identifier, Type>,
    /// acyclic mapping from Var(_n_) to more concrete type in type_vars
    type_vars: VarMap,
}

pub enum UnifyErr { Inf, Mis }

impl Context {
    pub fn new() -> Self {
        use super::Type::*;
        let fun = super::Type::fun;
        
        let mut bound = HashMap::with_capacity(10);
        let mut aliased = HashMap::with_capacity(10);

        bound.insert(format!("Prelude.+"), fun(Nat, fun(Nat, Nat)));
        aliased.insert(format!("+"), format!("Prelude.+"));

        bound.insert(format!("Prelude.chr"), fun(Nat, Char));
        aliased.insert(format!("chr"), format!("Prelude.chr"));

        bound.insert(format!("Prelude.ord"), fun(Char, Nat));
        aliased.insert(format!("ord"), format!("Prelude.ord"));

        Self {
            bound,
            aliased,
        }
    }

    pub(crate) fn insert(&mut self, k: String, v: Type) {
        self.bound.insert(k, v.flattened());
    }

    pub(crate) fn get(&self, k: &String) -> Option<&Type> {
        self.bound
            .get(self.aliased.get(k).unwrap_or(k))
    }

    pub(crate) fn has(&self, k: &String) -> bool {
        self.bound
            .contains_key(self.aliased.get(k).unwrap_or(k))
    }

    pub(crate) fn keys(&self) -> Vec<&String> {
        self.aliased.keys().chain(self.bound.keys()).collect()
    }
}

impl Solver {
    pub(crate) fn new(ctxt: Context) -> Self {
        Self {
            ctxt,
            locals: HashMap::new(),
            type_vars: HashMap::new()
        }
    }

    pub(crate) fn finish(self) -> Context {
        let Self { 
            ctxt: mut out, 
            locals, 
            type_vars } = self;
        
        for (k, v) in locals.iter() {
            out.insert(k.clone(), Self::_query(&type_vars, v));
        }
        
        out
    }

    pub(crate) fn get(self, k: &String) -> (Self, Option<Type>) {
        let t = self.locals.get(k).map(|t| self.query(t));
        if t.is_some() { return (self, t); }
        if let Some(t) = self.ctxt.get(k) {
            let (slvr, t) = t.clone().instanced(self);
            (slvr, Some(t))
        } else {
            (self, None)
        }
    }

    pub(crate) fn has(&self, k: &String) -> bool {
        self.locals.contains_key(k) || self.ctxt.has(k)
    }

    fn query_tvar(type_vars: &VarMap, var: usize) -> Type {
        let mut curr = var;
        loop {
            match type_vars.get(&curr).unwrap() {
                None => return Type::Var(curr),
                Some(Type::Var(next)) => { curr = *next; }
                Some(t) => return Self::_query(type_vars, t),
            }
        }
    }

    pub(crate) fn query(&self, t: &Type) -> Type {
        Self::_query(&self.type_vars, t)
    }

    fn _query(type_vars: &VarMap, t: &Type) -> Type {
        match t {
            Type::Unit | Type::Nat | Type::Char => t.clone(),
            Type::Data(nm, ts) => 
                Type::Data(nm.clone(), ts.iter().map(|t| Self::_query(type_vars, t)).collect()),
            Type::Var(v) => Self::query_tvar(type_vars, *v),
            Type::Fun(p, r) => 
                Type::fun(Self::_query(type_vars, p), Self::_query(type_vars, r)),
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

    pub(crate) fn bind_to_tvar(self, name: String) -> (Self, usize) {
        let (mut slf, k) = self.new_tvar();
        slf.locals.insert(name, Type::Var(k));
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