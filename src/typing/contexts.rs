use std::collections::{HashMap, hash_map::{Keys, Values}};

use super::{Type, scheme::Scheme, data::Kind};

type Identifier = String;
type QualifiedIdentifier = String;


#[derive(Debug, Clone)]
struct SimpleContext<T> {
    bound: HashMap<QualifiedIdentifier, T>, 
    aliased: HashMap<Identifier, QualifiedIdentifier>,
}

pub enum UnifyErr { Inf, Mis }

impl<T> SimpleContext<T> {
    pub(crate) fn blank() -> Self {
        Self { bound: HashMap::with_capacity(64), aliased: HashMap::with_capacity(64) }
    }

    fn add_prelude(&mut self, s: &str, t: T) {
        let qualed = format!("Prelude.{s}");
        self.bound.insert(qualed.clone(), t);
        self.aliased.insert(String::from(s), qualed);
    }

    pub(crate) fn insert(&mut self, k: String, v: T) {
        self.bound.insert(k, v);
    }

    pub(crate) fn extend(&mut self, other: Self) {
        self.bound.extend(other.bound);
        self.aliased.extend(other.aliased);
    }

    pub(crate) fn get(&self, k: &String) -> Option<&T> {
        self.bound
            .get(self.aliased.get(k).unwrap_or(k))
    }

    pub fn keys(&self) -> std::iter::Chain<Keys<String, String>, Keys<String, T>> {
        self.aliased.keys().chain(self.bound.keys())
    }

    pub fn values(&self) -> Values<String, T> {
        self.bound.values()
    }

    pub(crate) fn contains_key(&self, k: &String) -> bool {
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

        out.add_prelude("Unit", Kind::Type(Scheme::concrete(Type::unit())));
        out.add_prelude("Nat",  Kind::Type(Scheme::concrete(Type::nat())));
        out.add_prelude("Char", Kind::Type(Scheme::concrete(Type::char())));

        out
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    vars: SimpleContext<Scheme>,
    types: SimpleContext<Kind>,
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

    pub(crate) fn insert_type(&mut self, k: String, v: Kind) {
        self.types.insert(k, v);
    }

    pub(crate) fn get_type(&self, k: &String) -> Option<&Kind> {
        self.types.get(k)
    }

    // pub(crate) fn contains_type(&self, k: &String) -> bool {
    //     self.types.contains_key(k)
    // }

    // pub(crate) fn get_typekinds(&self) -> Values<String, Kind> {
    //     self.types.values()
    // }

    // pub(crate) fn get_typenames(&self) -> std::iter::Chain<Keys<String, String>, Keys<String, Kind>> {
    //     self.types.keys()
    // }

    pub(crate) fn insert_var(&mut self, k: String, v: Scheme) {
        self.vars.insert(k, v);
    }

    pub(crate) fn get_var(&self, k: &String) -> Option<&Scheme> {
        self.vars.get(k)
    }

    pub fn get_varnames(&self) -> std::iter::Chain<Keys<String, String>, Keys<String, Scheme>> {
        self.vars.keys()
    }

    pub(crate) fn get_vartypes(&self) -> Values<String, Scheme> {
        self.vars.values()
    }

    pub(crate) fn contains_var(&self, k: &String) -> bool {
        self.vars.contains_key(k)
    }
}