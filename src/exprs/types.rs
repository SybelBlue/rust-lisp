use std::fmt::Write;

use crate::parsing::FilePos;

use super::{contexts::TypeContext, values::Value, Expr, SBody};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    Unit,
    Int,
    Str,
    Type,
    Data(String),
    Fun(Box<Type>, Box<Type>),
    Var(String),
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
            },
            Type::Var(s) => write!(f, "t_{}", s),
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

pub fn type_expr<'a>(e: &'a Expr, ctxt: TypeContext) -> Result<(Type, TypeContext), TypeError<'a>> {
    match e {
        Expr::Val(v) => Ok(match v {
            Value::Int(_) => (Type::Int, ctxt),
            Value::Type(_) => (Type::Type, ctxt),
            Value::Sym(k) => (ctxt
                .get(k)
                .ok_or(TypeError::UndefinedSymbol(k))?
                .clone(), ctxt),
            Value::Lam(ps, b) => {
                let mut ctxt = ctxt.clone();
                let mut expr_type = Vec::new();
                for p in ps.as_ref().get_symbols() {
                    let (new, var) = ctxt.put_new_tvar(p);
                    ctxt = new;
                    expr_type.push(var);
                }
                let (ctxt, _ret_type) = ctxt.put_new_tvar(String::from("lam"));
                // let lam_type = expr_type.into_iter().rev().fold(ret_type, |arr, curr| Type::fun(curr, arr));
                // lam_type
                type_expr(b, ctxt)?
            },
        }),
        Expr::SExp(SBody { start, body }) => {
            if let Some((fst, rst)) = body.split_first() {
                let (mut f_type, mut ctxt) = type_expr(fst, ctxt)?;
                for e in rst {
                    let (e_type, new) = type_expr(e, ctxt)?;
                    ctxt = new;
                    if let Type::Fun(param_type, ret_type) = f_type.clone() {
                        if *param_type != e_type {
                            return Err(TypeError::TypeMismatch {
                                got: e_type,
                                expected: *param_type,
                                at: start,
                            });
                        } else {
                            f_type = *ret_type;
                        }
                    } else {
                        return Err(TypeError::TooManyArgs(start, fst));
                    }
                }
                Ok((f_type, ctxt))
            } else {
                Ok((Type::Unit, ctxt))
            }
        }
    }
}
