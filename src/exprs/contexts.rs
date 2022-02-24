use std::collections::HashMap;

use crate::interpreting::interpret;

use super::{types::{Type, type_expr, TypeError}, values::Value, Expr};

pub trait Implemented<'a> {
    fn eval(&'a self, ctxt: &mut Context<'a>) -> Value<'a>;
}

impl<'a> Implemented<'a> for Value<'a> {
    fn eval(&'a self, _: &mut Context<'a>) -> Value<'a> { self.clone() }
}

impl<'a> Implemented<'a> for &Expr<'a> {
    fn eval(&'a self, ctxt: &mut Context<'a>) -> Value<'a> { interpret(self, ctxt) }
}

struct BuiltInTodo(&'static str);

impl<'a> Implemented<'a> for BuiltInTodo {
    fn eval(&'a self, _: &mut Context<'a>) -> Value<'a> { todo!("Tried to exec {}", self.0) }
}

struct ContextEntry<'a> {
    tipe: Type,
    body: Box<(dyn Implemented<'a> + 'a)>,
}

impl<'a> ContextEntry<'a> {
    pub fn new(tipe: Type, body: &'a Expr<'a>) -> Self {
        Self { tipe, body: Box::new(body) }
    }
    pub fn todo(tipe: Type, name: &'static str) -> (String, Self) {
        (String::from(name), Self { tipe, body: Box::new(BuiltInTodo(name)) } )
    }
}

pub type Ident = String;
pub type QualifiedIdent = String;
pub struct Context<'a> {
    symbols: HashMap<QualifiedIdent, ContextEntry<'a>>,
    imported: HashMap<Ident, QualifiedIdent>,
}

impl Context<'static> {
    pub fn new() -> Self {
        use super::types::Type::*;
        Self { 
            symbols: vec!
                [ ContextEntry::todo(Fun(Box::new(Str), Box::new(Type)), "let")
                , ContextEntry::todo(Fun(Box::new(Int), Box::new(Fun(Box::new(Int), Box::new(Int)))), "+")
                ].into_iter().collect(),
            imported: HashMap::new(),
        }
    }
}

impl<'a> Context<'a> {
    fn get(&self, key: &String) -> Option<&ContextEntry<'a>> {
        let qual = self.imported.get(key).unwrap_or(key);
        self.symbols.get(qual)
    }

    pub fn get_type(&self, key: &String) -> Option<&Type> {
        self.get(key).map(|entry| &entry.tipe)
    }

    pub fn get_impl(&'a self, key: &String) -> Option<&'a dyn Implemented<'a>> {
        self.get(key).map(|entry| entry.body.as_ref())
    }

    pub fn define(&mut self, key: String, empl: &'a Expr<'a>) -> Result<(), TypeError> {
        let tipe = type_expr(empl, self)?;
        self.symbols.insert(key, ContextEntry::new(tipe, empl));
        Ok(())
    }
}