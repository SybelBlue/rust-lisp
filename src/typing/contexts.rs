use std::{collections::{hash_map::Keys, HashMap}, sync::Arc};

use crate::parsing::ArcStr;

use super::{Type, scheme::Scheme};

pub type Identifier = ArcStr;
pub type QualifiedIdentifier = ArcStr;


#[derive(Debug, Clone)]
pub struct Context {
    /// Types are compressed, ie all Var(_n_) inside the type start a 0
    bound: HashMap<QualifiedIdentifier, Scheme>,
    aliased: HashMap<Identifier, QualifiedIdentifier>,
}

pub enum UnifyErr { Inf, Mis }

impl Context {
    fn blank() -> Self {
        Self { bound: HashMap::with_capacity(128), aliased: HashMap::with_capacity(128) }
    }

    fn add_prelude(&mut self, s: &str, sc: Scheme) {
        let qualed: ArcStr = format!("Prelude.{s}").into();
        self.bound.insert(qualed.clone(), sc);
        self.aliased.insert(Arc::from(s), qualed);
    }

    pub fn new() -> Self {
        let mut out = Self::blank();

        out.add_prelude("+",   Scheme::concrete(Type::fun(Type::nat(), Type::fun(Type::nat(), Type::nat()))));
        out.add_prelude("chr", Scheme::concrete(Type::fun(Type::nat(), Type::char())));
        out.add_prelude("ord", Scheme::concrete(Type::fun(Type::char(), Type::nat())));

        out
    }

    pub fn insert(&mut self, k: Identifier, v: Scheme) {
        self.bound.insert(k, v);
    }

    pub fn get(&self, k: &Identifier) -> Option<&Scheme> {
        self.bound
            .get(self.aliased.get(k).unwrap_or(k))
    }

    pub fn keys(&self) -> std::iter::Chain<Keys<Identifier, QualifiedIdentifier>, Keys<QualifiedIdentifier, Scheme>> {
        self.aliased.keys().chain(self.bound.keys())
    }

    pub fn contains_key(&self, k: &Identifier) -> bool {
        self.bound.contains_key(k)
    }
}