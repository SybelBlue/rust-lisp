use crate::{builtin_fn::BuiltInFn, result::FilePos, value::Value::*, token::Token};


pub fn form_string(_form: &[Token]) -> String {
    // form.iter().map(|t| t.data.clone()).collect::<Vec<String>>().join(" ")
    unimplemented!()
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

// #[derive(Debug, Clone, PartialEq)]
// pub struct Class {
//     name: Ident,
//     vars: Vec<Ident>,
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Usr(Ident, Vec<Type>),
    Var(String),
    Arrow(Ident, Box<Type>, Box<Type>),
    // Constraint(Class, Box<Type>)
}

#[derive(Debug, Clone)]
pub enum Value {
    Fn(Type, String, Box<Expr>),
    BuiltIn(BuiltInFn),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    // pub tp: Type,
    // pub expr: ExprBody
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Fn(tp, _, _) => write!(f, "<func: {:?}>", tp),
            BuiltIn(bifn) => write!(f, "<builtin func {}>", bifn.name),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Fn(_, r, x), 
                Fn(_, s, y)) => 
                    r == s && x == y,
            (Fn(_, _, _), _) => false, 
            (BuiltIn(x), 
                BuiltIn(y)) => x == y,
            (BuiltIn(_), _) => false,
        }
    }
}
