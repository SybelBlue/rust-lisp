use std::collections::VecDeque;

use crate::{builtin_fn::BuiltInFn, context::Context, evaluator::{*, value::Value::*, expr::*}};


pub fn form_string(form: &[Token]) -> String {
    form.iter().map(|t| format!("{}", t)).collect::<Vec<String>>().join(" ")
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
    pub file_pos: FilePos,
}

impl Ident {
    pub fn new(name: String, file_pos: FilePos) -> Self {
        Self { name, file_pos }
    }
}

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}", self.name, self.file_pos)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Fn(Vec<Ident>, Option<Ident>, Box<Token>),
    BuiltIn(BuiltInFn),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unit => write!(f, "()"),
            Int(x) => write!(f, "{}", x),
            Fn(args, Some(_), _) => write!(f, "<({}+n)-ary func>", args.len()),
            Fn(args, None, _) => write!(f, "<{}-ary func>", args.len()),
            BuiltIn(bifn) => write!(f, "<builtin func {}>", bifn.name),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Int(x), Int(y)) => x == y,
            (Int(_), _) => false,
            (Fn(_, r, x), 
                Fn(_, s, y)) => 
                    r == s && x.expr == y.expr,
            (Fn(_, _, _), _) => false, 
            (BuiltIn(x), 
                BuiltIn(y)) => x == y,
            (BuiltIn(_), _) => false,
        }
    }
}
