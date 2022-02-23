use std::collections::{HashSet, HashMap};

use crate::parsing::{Value, Expr};

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

impl Type {
    pub fn fun(par: Self, ret: Self) -> Self {
        Self::Fun(Box::new(par), Box::new(ret))
    }
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

impl TypeContext {
    pub fn new() -> Self {
        Self { symbols: HashMap::new() }
    }

    pub fn get_or_decl(&mut self, key: &String) -> &Type {
        self.symbols.get(key).expect("make polymorphic type vars")
    }
}

#[derive(Debug)]
pub enum TypeError<'a> {
    TooManyArgs(&'a Expr),
    TypeMismatch { got: Type, expected: Type }
}

pub fn type_expr<'a>(e: &'a Expr, ctxt: &mut TypeContext) -> Result<Type, TypeError<'a>> {
    match e {
        Expr::Val(v) => Ok(match v {
            Value::Int(_) => Type::Int,
            Value::Type(_) => Type::Type,
            Value::Sym(k) => ctxt.get_or_decl(k).clone(),
            Value::Quot(e) => Type::fun(Type::Unit, type_expr(e.as_ref(), ctxt)?),
        }),
        Expr::SExp(es) => {
            if let Some(f) = es.first() {
                let mut f_type = type_expr(f, ctxt)?;
                for e in &es[1..] {
                    let e_type = type_expr(e, ctxt)?;
                    if let Type::Fun(pt, rt) = f_type.clone() {
                        if *pt != e_type {
                            return Err(TypeError::TypeMismatch{ got: e_type, expected: *pt });
                        } else {
                            f_type = *rt;
                        }
                    } else {
                        return Err(TypeError::TooManyArgs(f));
                    }
                }
                Ok(f_type)
            } else {
                Ok(Type::Unit)
            }

        },
    }
}