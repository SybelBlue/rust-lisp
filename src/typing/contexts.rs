use std::collections::{HashMap, hash_map::{Keys, Values}};
use std::sync::Arc;

use crate::data::Kind;

use super::{Type, scheme::Scheme};

type Identifier = Arc<str>;
type QualifiedIdentifier = Arc<str>;


#[derive(Debug, Clone)]
pub(crate) struct SimpleContext<T> {
    bound: HashMap<QualifiedIdentifier, T>,
    aliased: HashMap<Identifier, QualifiedIdentifier>,
}

pub enum UnifyErr { Inf, Mis }

impl<T> SimpleContext<T> {
    pub(crate) fn blank() -> Self {
        Self { bound: HashMap::with_capacity(64), aliased: HashMap::with_capacity(64) }
    }

    fn add_prelude(&mut self, s: &str, t: T) {
        let qualed: Arc<str> = Arc::from(format!("Prelude.{s}"));
        self.bound.insert(qualed.clone(), t);
        self.aliased.insert(Arc::from(s), qualed);
    }

    pub(crate) fn insert(&mut self, k: QualifiedIdentifier, v: T) {
        self.bound.insert(k, v);
    }

    pub(crate) fn extend(&mut self, other: Self) {
        self.bound.extend(other.bound);
        self.aliased.extend(other.aliased);
    }

    pub(crate) fn get(&self, k: &Identifier) -> Option<&T> {
        self.bound
            .get(self.aliased.get(k).unwrap_or(k))
    }

    pub fn keys(&self) -> std::iter::Chain<Keys<Identifier, QualifiedIdentifier>, Keys<QualifiedIdentifier, T>> {
        self.aliased.keys().chain(self.bound.keys())
    }

    pub fn values(&self) -> Values<QualifiedIdentifier, T> {
        self.bound.values()
    }

    pub(crate) fn contains_key(&self, k: &QualifiedIdentifier) -> bool {
        self.bound.contains_key(k)
    }
}

impl SimpleContext<Scheme> {
    fn new_scheme_ctxt() -> Self {
        let mut out = Self::blank();

        out.add_prelude("+",   Scheme::concrete(Type::fun(Type::nat(), Type::fun(Type::nat(), Type::nat()))));
        out.add_prelude("chr", Scheme::concrete(Type::fun(Type::nat(), Type::char())));
        out.add_prelude("ord", Scheme::concrete(Type::fun(Type::char(), Type::nat())));

        out
    }
}

impl SimpleContext<Kind> {
    fn new_kind_ctxt() -> Self {
        let mut out = Self::blank();

        out.add_prelude("Unit", Kind::Type);
        out.add_prelude("Nat",  Kind::Type);
        out.add_prelude("Char", Kind::Type);

        out
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    pub(crate) vars: SimpleContext<Scheme>,
    pub(crate) types: SimpleContext<Kind>,
}

impl Context {
    pub(crate) fn blank() -> Self {
        Self { vars: SimpleContext::blank(), types: SimpleContext::blank() }
    }

    pub fn new() -> Self {
        Self {
            vars: SimpleContext::new_scheme_ctxt(),
            types: SimpleContext::new_kind_ctxt(),
        }
    }

    pub(crate) fn extend(&mut self, other: Self) {
        self.vars.extend(other.vars);
        self.types.extend(other.types);
    }

    pub(crate) fn insert_type(&mut self, k: Identifier, v: Kind) {
        self.types.insert(k, v);
    }

    pub(crate) fn get_type(&self, k: &Identifier) -> Option<&Kind> {
        self.types.get(k)
    }

    // pub(crate) fn contains_type(&self, k: &String) -> bool {
    //     self.types.contains_key(k)
    // }

    // pub(crate) fn get_typekinds(&self) -> Values<String, Kind<Type>> {
    //     self.types.values()
    // }

    // pub(crate) fn get_typenames(&self) -> std::iter::Chain<Keys<String, String>, Keys<String, Kind<Type>>> {
    //     self.types.keys()
    // }

    pub(crate) fn insert_var(&mut self, k: Identifier, v: Scheme) {
        self.vars.insert(k, v);
    }

    pub(crate) fn get_var(&self, k: &Identifier) -> Option<&Scheme> {
        self.vars.get(k)
    }

    pub fn get_varnames(&self) -> std::iter::Chain<Keys<Identifier, QualifiedIdentifier>, Keys<QualifiedIdentifier, Scheme>> {
        self.vars.keys()
    }

    pub(crate) fn get_vartypes(&self) -> Values<QualifiedIdentifier, Scheme> {
        self.vars.values()
    }

    pub(crate) fn contains_var(&self, k: &Identifier) -> bool {
        self.vars.contains_key(k)
    }
}