use std::collections::HashMap;

// use crate::interpreting::interpret;

use super::types::Type;

// pub trait Implemented<'a> {
//     fn eval(&'a self, ctxt: &mut GenContext<'a>) -> Value<'a>;
// }

// impl<'a> Implemented<'a> for Value<'a> {
//     fn eval(&'a self, _: &mut GenContext<'a>) -> Value<'a> { self.clone() }
// }

// impl<'a> Implemented<'a> for Expr<'a> {
//     fn eval(&'a self, ctxt: &mut GenContext<'a>) -> Value<'a> { interpret(self, ctxt) }
// }

// struct BuiltInTodo(&'static str);

// impl<'a> Implemented<'a> for BuiltInTodo {
//     fn eval(&'a self, _: &mut GenContext<'a>) -> Value<'a> { todo!("Tried to exec {}", self.0) }
// }

// struct ContextEntry<'a> {
//     tipe: Type,
//     body: Box<(dyn Implemented<'a> + 'a)>,
// }

// impl<'a> ContextEntry<'a> {
//     pub fn new(tipe: Type, body: Expr<'a>) -> Self {
//         Self { tipe, body: Box::new(body) }
//     }
//     pub fn todo(name: &'static str, tipe: Type) -> (String, Self) {
//         (String::from(name), Self { tipe, body: Box::new(BuiltInTodo(name)) } )
//     }
// }

pub type Ident = String;
pub type QualifiedIdent = String;
pub struct GenContext<T> {
    symbols: HashMap<QualifiedIdent, T>,
    imported: HashMap<Ident, QualifiedIdent>,
}

impl GenContext<Type> {
    pub fn new() -> Self {
        use super::types::Type::*;
        Self { 
            symbols: vec!
                [ (format!("let"), Fun(Box::new(Str), Box::new(Type)))
                , (format!("+"), Fun(Box::new(Int), Box::new(Fun(Box::new(Int), Box::new(Int)))))
                ]
                .into_iter().collect(),
            imported: HashMap::new(),
        }
    }
}

impl<T> GenContext<T> {
    pub fn get(&self, key: &String) -> Option<&T> {
        self.symbols.get(self.qualify(key))
    }

    pub fn qualify<'a>(&'a self, key: &'a String) -> &'a String {
        self.imported.get(key).unwrap_or(key)
    }

    // pub fn get_type(&self, key: &String) -> Option<&Type> {
    //     self.get(key).map(|entry| &entry.tipe)
    // }

    // pub fn get_impl(&'a self, key: &String) -> Option<&'a dyn Implemented<'a>> {
    //     self.get(key).map(|entry| entry.body.as_ref())
    // }

    // pub fn define(&mut self, key: String, empl: &'a Expr<'a>) -> Result<(), TypeError> {
    //     let tipe = type_expr(empl, self)?;
    //     self.symbols.insert(key, ContextEntry::new(tipe, empl.clone()));
    //     Ok(())
    // }
}











#[derive(Debug, Clone)]
pub enum TypeContext {
    Empty,
    Entry {
        symb: Ident,
        tipe: Type,
        base: Box<TypeContext>,
        tvar: usize,
    }
}

impl TypeContext {
    pub fn get<'a>(&'a self, key: &Ident) -> Option<&'a Type> {
        match self {
            Self::Empty => None,
            Self::Entry { symb, tipe, base, ..} => {
                if symb == key {
                    Some(tipe)
                } else {
                    base.as_ref().get(key)
                }
            }
        }
    }

    fn tvar(&self) -> usize {
        if let Self::Entry { tvar, .. } = self {
            *tvar
        } else {
            0
        }
    }

    pub fn put(self, symb: Ident, tipe: Type) -> Self {
        let tvar = self.tvar();
        Self::Entry { symb, tipe, base: Box::new(self), tvar }
    }

    pub fn put_new_tvar(self, symb: Ident) -> (Self, Type) {
        let tvar = self.tvar();
        let tipe = Type::Var(format!("{}_{}", symb, tvar));
        let tvar = tvar + 1;
        (Self::Entry { symb, tipe: tipe.clone(), base: Box::new(self), tvar }, tipe)
    }
}