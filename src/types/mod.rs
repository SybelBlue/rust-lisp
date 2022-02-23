use std::collections::{HashSet, HashMap};

use crate::parsing::lex::Token;

pub type TVar = String;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    Unit,
    Int,
    Str,
    Type,
    Data(String),
    Fun(Box<Type>, Box<Type>)
}

// pub enum PolyTypeBody {
//     Con(ConcType),
//     Var(TVar),
//     Fun(Box<Type>, Box<Type>),
// }

// pub enum Type {
    // C(ConcType),
    // P(Vec<Constraint>, PolyTypeBody),
// }

#[derive(Debug, Clone)]
pub enum Constraint {
    Eq(HashSet<TVar>),
    // Trait(TVar, Vec<Type>),
}

pub struct TypeContext {
    symbols: HashMap<String, Type>
}

pub fn type_token<'a>(t: &Token, ctxt: &'a mut TypeContext) -> &'a Type {
    todo!()
}