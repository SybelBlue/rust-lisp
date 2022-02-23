use std::collections::HashMap;

use super::{types::Type, Expr};


pub struct Context {
    symbols: HashMap<String, Type>
}

impl Context {
    pub fn new() -> Self {
        use super::types::Type::*;
        Self { symbols: vec!
                [ (format!("let"), Fun(Box::new(Str), Box::new(Type)))
                , (format!("+"), Fun(Box::new(Int), Box::new(Fun(Box::new(Int), Box::new(Int)))))
                ].into_iter().collect(),
        }
    }

    pub fn get_or_decl(&mut self, key: &String) -> &Type {
        self.symbols.get(key).expect("make polymorphic type vars")
    }
}