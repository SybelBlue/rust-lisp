use crate::{builtin_fn::BuiltInFn, expr::Expr, result::FilePos, rtype::Type, value::Value::*};

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
pub enum Value {
    // always well-typed
    Int(i64),
    Variant(Type, Ident, Vec<Value>),
    Lambda(Type, String, Box<Expr>),
    BuiltIn(BuiltInFn),
}

impl Value {
    pub fn depth(&self) -> usize {
        match self {
            Lambda(_, _, e) => e.depth() + 1,
            BuiltIn(bifn) => bifn.tp.depth(),
            _ => 0,
        }
    }

    pub fn get_type<'a>(&'a self) -> &'a Type {
        match self {
            Self::Int(_) => &Type::Int,
            Self::Variant(t, _, _) => t,
            Self::Lambda(t, _, _) => t,
            Self::BuiltIn(bifn) => &bifn.tp,
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
