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

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    name: Ident,
    vars: Vec<Ident>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Enum(Ident, Vec<Type>), // just the name and type vars
    TVar(String),
    Arrow(Ident, Box<Type>, Box<Type>),
    Constraint(Class, Box<Type>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value { // always well-typed
    Int(i64),
    Variant(Type, Ident, Vec<Value>),
    Lambda(Type, String, Box<Expr>),
    BuiltIn(BuiltInFn),
}

impl Value {
    pub fn depth(&self) -> usize {
        match self {
            Lambda(_, _, e) => e.depth() + 1,
            BuiltIn(_bifn) => 0, // todo: fix this
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Val(Value),
    Var(String),
    Form(Vec<Expr>),
}

impl Expr {
    pub fn depth(&self) -> usize {
        if let Expr::Val(v) = self {
            v.depth()
        } else {
            0
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Int(n) => write!(f, "{}", n),
            Variant(_t, idn, vals) => write!(f, "{} {:?}", idn, vals),
            Lambda(_t, _n, e) => write!(f, "<{}-ary func>", e.depth()),
            BuiltIn(bifn) => write!(f, "<builtin func {}>", bifn.name),
        }
    }
}