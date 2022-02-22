use std::collections::HashMap;

pub type TVar<'a> = &'a String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'a> {
    Unit,
    Int,
    Str,
    Type,
    Var(TVar<'a>),
    Fun(Box<Type<'a>>, Box<Type<'a>>)
}

#[derive(Debug, Clone)]
pub enum Constraint<'a> {
    Is(TVar<'a>, Type<'a>),
    // Trait(TVar, Vec<Type>),
}

pub struct TypeContext<'a> {
    known: HashMap<String, Type<'a>>,
    constraints: Vec<Constraint<'a>>
}

impl<'a> TypeContext<'a> {
    pub fn new() -> Self {
        Self { known: HashMap::new(), constraints: Vec::new() }
    }

    pub fn update<F>(&'a mut self, name: String, default: Type<'a>, f: F) -> &'a mut Type where F: FnOnce(&mut Type) {
        self.known.entry(name).and_modify(f).or_insert(default)
    }
}
