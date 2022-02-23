use std::collections::HashMap;

use super::{types::Type, values::Value};

pub(crate) trait Implemented {
    fn eval<'a>(&'a self) -> &'a Value<'a>;
}

struct ContextEntry {
    tipe: Type,
    empl: Box<dyn Implemented>,
}

impl ContextEntry {
    pub fn new<I: 'static + Implemented>(tipe: Type, empl: I) -> Self {
        Self {
            tipe,
            empl: Box::new(empl)
        }
    }
}

impl Implemented for ContextEntry {
    fn eval<'a>(&'a self) -> &'a Value<'a> {
        self.empl.as_ref().eval()
    }
}

struct BuiltInTodo(&'static str);

impl Implemented for BuiltInTodo {
    fn eval<'a>(&'a self) -> &'a Value<'a> {
        todo!("{} not yet implemented", self.0)
    }
}

pub struct Context {
    symbols: HashMap<String, ContextEntry>
}

impl Context {
    pub fn new() -> Self {
        use super::types::Type::*;
        Self { 
            symbols: vec!
                [ (format!("let"), ContextEntry::new(Fun(Box::new(Str), Box::new(Type)), BuiltInTodo("let")))
                , (format!("+"), ContextEntry::new(Fun(Box::new(Int), Box::new(Fun(Box::new(Int), Box::new(Int)))), BuiltInTodo("+")))
                ].into_iter().collect(),
        }
    }

    pub fn get_type(&mut self, key: &String) -> &Type {
        if let Some(entry) = self.symbols.get(key) {
            &entry.tipe
        } else {
            panic!("make polymorphic types")
        }
    }
}