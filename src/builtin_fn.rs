use std::collections::{HashMap, VecDeque};

use crate::{
    context::{Context, CtxtMap, CtxtMapValue}, 
    evaluator::{
        *, 
        expr::Expr, 
        result::{*, Error::*}, 
        token::Token, 
        value::{Value::{self, *}
    }
}};

#[derive(Clone)]
pub struct BuiltInFn {
    pub name: String, 
    pub f: fn(&Context, Vec<Token>) -> EvalResult<Value>,
}

impl BuiltInFn {
    pub fn new(name: &str, f: fn(&Context, Vec<Token>) -> EvalResult<Value>) -> Value {
        Value::BuiltIn(Self::simple(String::from(name), f))
    }

    pub fn simple(name: String, f: fn(&Context, Vec<Token>) -> EvalResult<Value>) -> Self {
        BuiltInFn { name, f }
    }
}

impl std::fmt::Debug for BuiltInFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BIBody({})", self.name)
    }
}

impl std::cmp::PartialEq for BuiltInFn {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
