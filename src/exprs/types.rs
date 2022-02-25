use std::fmt::Write;

use crate::parsing::FilePos;

use super::{contexts::Context, values::Value, Expr};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    Unit,
    Int,
    Str,
    Type,
    Data(String),
    Fun(Box<Type>, Box<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Int => write!(f, "Int"),
            Type::Str => write!(f, "String"),
            Type::Type => write!(f, "Type"),
            Type::Data(s) => write!(f, "{}", s),
            Type::Fun(pt, rt) => {
                write!(f, "(-> {}", pt.as_ref())?;
                for t in rt.as_ref().unpack() {
                    write!(f, " {}", t)?;
                }
                f.write_char(')')
            }
        }
    }
}

impl Type {
    pub fn fun(par: Self, ret: Self) -> Self {
        Self::Fun(Box::new(par), Box::new(ret))
    }

    fn unpack(&self) -> Vec<&Self> {
        if let Self::Fun(pt, rt) = self {
            let mut out = vec![pt.as_ref()];
            out.extend(rt.as_ref().unpack());
            out
        } else {
            vec![&self]
        }
    }
}

#[derive(Debug)]
pub enum TypeError<'a> {
    TooManyArgs(&'a FilePos<'a>, &'a Expr<'a>),
    TypeMismatch {
        got: Type,
        expected: Type,
        at: &'a FilePos<'a>,
    },
    UndefinedSymbol(&'a String),
}

pub fn type_expr<'a>(e: &'a Expr, ctxt: &mut Context) -> Result<Type, TypeError<'a>> {
    type_expr_helper(e, ctxt)
}

fn type_expr_helper<'a>(e: &'a Expr, ctxt: &mut Context) -> Result<Type, TypeError<'a>> {
    match e {
        Expr::Val(v) => Ok(match v {
            Value::Int(_) => Type::Int,
            Value::Type(_) => Type::Type,
            Value::Sym(k) => ctxt
                .get_type(k)
                .ok_or(TypeError::UndefinedSymbol(k))?
                .clone(),
            Value::Quot(e) => Type::fun(Type::Unit, type_expr(e.as_ref(), ctxt)?),
        }),
        Expr::SExp(at, es) => {
            if let Some((fst, rst)) = es.split_first() {
                let mut f_type = type_expr(fst, ctxt)?;
                for e in rst {
                    let e_type = type_expr(e, ctxt)?;
                    if let Type::Fun(param_type, ret_type) = f_type.clone() {
                        if *param_type != e_type {
                            return Err(TypeError::TypeMismatch {
                                got: e_type,
                                expected: *param_type,
                                at,
                            });
                        } else {
                            f_type = *ret_type;
                        }
                    } else {
                        return Err(TypeError::TooManyArgs(at, fst));
                    }
                }
                Ok(f_type)
            } else {
                Ok(Type::Unit)
            }
        }
    }
}
