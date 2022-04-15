use crate::parsing::FilePos;

use super::{contexts::TypeContext, values::Value, Expr, SBody};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Unit,
    Nat,
    Char,
    Type,
    Data(String),
    Var(usize),
    Fun(Box<Type>, Box<Type>),
}

impl Type {
    pub(crate) fn fun(p: Self, r: Self) -> Self {
        Self::Fun(Box::new(p), Box::new(r))
    }

    fn unify(&self, got: &Self, ctxt: TypeContext) -> Option<TypeContext> {
        if self == got { return Some(ctxt); }
        if let Type::Var(s) = self {
            return Some(ctxt.put_eq(*s, got.clone()));
        } else if let Type::Var(g) = got {
            return Some(ctxt.put_eq(*g, self.clone()))
        } 
        None
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type => true,
            Type::Data(_) => true, // will maybe be polymorphic later
            Type::Fun(p, r) => p.as_ref().is_concrete() && r.as_ref().is_concrete(),
            Type::Var(_) => false,
        }
    }

    pub fn concretize(self, ctxt: TypeContext) -> Result<(Self, TypeContext), TypeError<'static>> {
        Ok(match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type => (self, ctxt),
            Type::Data(_) => (self, ctxt), // will maybe be polymorphic later
            Type::Fun(p, r) => {
                let (p, ctxt) = p.concretize(ctxt)?;
                let (r, ctxt) = r.concretize(ctxt)?;
                (Type::fun(p, r), ctxt)
            },
            Type::Var(id) => ctxt.concretize(id)?,
        })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Nat => write!(f, "Nat"),
            Type::Char => write!(f, "Char"),
            Type::Type => write!(f, "Type"),
            Type::Data(s) => write!(f, "{}", s),
            Type::Fun(pt, rt) => 
                write!(f, "(-> {} {})", pt.as_ref(), rt.as_ref()),
            Type::Var(s) => write!(f, "t_{}", s),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeError<'a> {
    TooManyArgs(&'a FilePos<'a>, &'a Expr<'a>),
    TypeMismatch {
        got: Type,
        expected: Type,
        at: &'a FilePos<'a>,
    },
    BadEquivalence(Type, Type),
    UndefinedSymbol(&'a String),
}

pub fn type_expr<'a>(e: &'a Expr, ctxt: TypeContext) -> Result<(Type, TypeContext), TypeError<'a>> {
    match e {
        Expr::Val(v) => Ok(match v {
            Value::Int(_) => (Type::Nat, ctxt),
            Value::Type(_) => (Type::Type, ctxt),
            Value::Sym(k) => (ctxt
                .get(k)
                .ok_or(TypeError::UndefinedSymbol(k))?
                .clone(), ctxt),
            Value::Lam(ps, b) => {
                let mut ctxt = ctxt.clone();
                let mut expr_type = Vec::new(); // in reverse order!
                for p in ps.as_ref().get_symbols() {
                    let (new, var) = ctxt.put_new_tvar(p);
                    ctxt = new;
                    expr_type.push(Type::Var(var));
                }
                let (ctxt, ret_type_var) = ctxt.put_new_tvar(String::from("anon"));
                let (ret_type, ctxt) = type_expr(b, ctxt)?;
                let ctxt = ctxt.put_eq(ret_type_var, ret_type.clone());
                // undoes reversal!
                let lam_type = expr_type.into_iter().fold(ret_type, |arr, curr| Type::fun(curr, arr));
                let (t, ctxt) = lam_type.concretize(ctxt)?;
                (t, ctxt)
            },
        }),
        Expr::SExp(SBody { start, body }) => {
            if let Some((fst, rst)) = body.split_first() {
                let (mut target_type, mut ctxt) = type_expr(fst, ctxt)?;
                let mut rest = rst.into_iter();
                while let Some(curr_argument) = rest.next() {
                    let (arg_type, new) = type_expr(curr_argument, ctxt)?;
                    ctxt = new;
                    match target_type {
                        Type::Fun(param_type, ret_type) => {
                            if let Some(new) = param_type.unify(&arg_type, ctxt) {
                                target_type = *ret_type;
                                ctxt = new;
                            } else {
                                return Err(TypeError::TypeMismatch {
                                    got: arg_type,
                                    expected: *param_type,
                                    at: start,
                                });
                            }
                        },
                        Type::Var(id) => {
                            let (new, ret_type_id) = ctxt.put_new_tvar(String::from("anon"));
                            ctxt = new.put_eq(id, Type::fun(arg_type, Type::Var(ret_type_id)));
                            target_type = Type::Var(ret_type_id);
                        },
                        _ => return Err(TypeError::TooManyArgs(start, fst)),
                    }
                }
                // println!("{:?}", target_type);
                target_type.concretize(ctxt)
            } else {
                Ok((Type::Unit, ctxt))
            }
        }
    }
}
