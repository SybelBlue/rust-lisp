use crate::{context::{Context}, expr::Expr, result::*, rtype::Type, value::Value};

#[derive(Clone)]
pub struct BuiltInFn {
    pub name: String, 
    pub f: fn(&Context, Vec<Expr>) -> EvalResult<Value>,
    pub tp: Type,
}

impl BuiltInFn {
    pub fn new_val(name: &str, tp: Type, f: fn(&Context, Vec<Expr>) -> EvalResult<Value>) -> Value {
        Value::BuiltIn(Self::new(String::from(name), tp, f))
    }

    pub fn new(name: String, tp: Type, f: fn(&Context, Vec<Expr>) -> EvalResult<Value>) -> Self {
        BuiltInFn { name, tp, f }
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

pub fn add(_ctxt: &Context, _exprs: Vec<Expr>) -> EvalResult<Value> {
    todo!()
}
