use std::{collections::{HashSet, HashMap}, fmt::{Write, Display, Formatter}};

use crate::errors::{TypeResult, TypeError};

use super::{contexts::{TypeContext, UnifyErr}, values::Value, Expr, SBody};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
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

    pub fn contains(&self, o: &Self) -> bool {
        if self == o {
            true
        } else if let Type::Fun(p, r) = self {
            p.contains(o) || r.contains(o)
        } else {
            false
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type => true,
            Type::Data(_) => true, // will maybe be polymorphic later
            Type::Fun(p, r) => p.is_concrete() && r.is_concrete(),
            Type::Var(_) => false,
        }
    }

    pub fn concretize(&self, ctxt: &TypeContext) -> Self {
        let out = match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type | Self::Data(_) => self.clone(),
            Type::Fun(p, r) => 
                Type::fun(p.concretize(ctxt), r.concretize(ctxt)),
            Type::Var(id) => ctxt.query_tvar(*id),
        };
        if out == *self {
            out
        } else {
            out.concretize(ctxt)
        }
    }

    pub fn alpha_sub(&self, tvar: usize, sub: &Self) -> Self {
        match self {
            Type::Var(x) if *x == tvar => sub.clone(),
            Type::Fun(p, r) => Type::fun(
                p.alpha_sub(tvar, sub), 
                r.alpha_sub(tvar, sub)
            ),
            _ => self.clone(),
        }
    }

    pub fn variable_values(&self, out: &mut HashSet<usize>) {
        match self {
            Self::Var(n) => { out.insert(*n); },
            Self::Fun(p, r) => {
                p.variable_values(out);
                r.variable_values(out);
            },
            Self::Data(_) => todo!(),
            _ => {}
        }
    }

    pub(crate) fn var_to_char_map(mut vals: Vec<usize>) -> HashMap<usize, char> {
        vals.sort();
        let map: HashMap<usize, char> = vals.into_iter()
            .enumerate()
            .map(|(i, l)| (l, (i as u8 + 'a' as u8) as char))
            .collect();
        if map.len() > 26 { todo!("sooooo many vars") }
        map
    }

    pub(crate) fn display_with(&self, f: &mut Formatter, map: &HashMap<usize, char>, wrap: bool) -> std::fmt::Result {
        match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type | Type::Data(_) => self.fmt(f),
            Type::Var(n) => f.write_char(*map.get(n).unwrap()),
            Type::Fun(p, r) => {
                if wrap { f.write_char('(')?; }
                p.display_with(f, map, true)?;
                f.write_str(" -> ")?;
                r.display_with(f, map, false)?;
                if wrap { f.write_char(')')?; }
                Ok(())
            },
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Nat => write!(f, "Nat"),
            Type::Char => write!(f, "Char"),
            Type::Type => write!(f, "Type"),
            Type::Data(s) => write!(f, "{}", s),
            Type::Fun(_, _) => {
                let mut vals = HashSet::new();
                self.variable_values(&mut vals);
                self.display_with(f, &Type::var_to_char_map(vals.into_iter().collect()), true)
            },
            Type::Var(_) => f.write_char('a'),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "Unit"),
            Self::Nat => write!(f, "Nat"),
            Self::Char => write!(f, "Char"),
            Self::Type => write!(f, "Type"),
            Self::Data(arg0) => f.debug_tuple("Data").field(arg0).finish(),
            Self::Var(arg0) => write!(f, "t_{}", arg0),
            Self::Fun(arg0, arg1) => write!(f, "({:?} -> {:?})", arg0.as_ref(), arg1.as_ref()),
        }
    }
}

pub fn type_expr<'a>(e: &'a Expr, ctxt: TypeContext) -> TypeResult<'a, (Type, TypeContext)> {
    match e {
        Expr::Val(v) => type_value(v, ctxt),
        Expr::SExp(SBody { start, body }) => {
            if let Some((fst, rst)) = body.split_first() {
                let (mut target_type, mut ctxt) = type_expr(fst, ctxt)?;
                let mut rest = rst.into_iter();
                while let Some(curr_argument) = rest.next() {
                    let (arg_type, new) = type_expr(curr_argument, ctxt)?;
                    ctxt = new;
                    match target_type {
                        Type::Fun(param_type, ret_type) => {
                            match ctxt.unify(param_type.as_ref(), &arg_type) {
                                Ok(new) => {
                                    target_type = *ret_type;
                                    ctxt = new;
                                }
                                Err(UnifyErr::Mis) => {
                                    return Err(TypeError::TypeMismatch {
                                        got: arg_type,
                                        expected: *param_type,
                                        at: start,
                                    });
                                }
                                Err(UnifyErr::Inf) => {
                                    return Err(TypeError::InfiniteType(param_type.as_ref().clone(), arg_type));
                                }
                            }
                        }
                        Type::Var(_) => {
                            let (new, ret_type_id) = ctxt.new_tvar();
                            let curr_expr_type = Type::fun(arg_type, Type::Var(ret_type_id));
                            match new.unify(&target_type, &curr_expr_type) {
                                Ok(new) => {
                                    target_type = Type::Var(ret_type_id);
                                    ctxt = new;
                                }
                                Err(UnifyErr::Mis) => {
                                    return Err(TypeError::TypeMismatch {
                                        got: curr_expr_type,
                                        expected: target_type,
                                        at: start,
                                    });
                                }
                                Err(UnifyErr::Inf) => {
                                    return Err(TypeError::InfiniteType(target_type, curr_expr_type));
                                }
                            }
                        }
                        _ => return Err(TypeError::TooManyArgs(start, fst)),
                    }
                }
                Ok((target_type.concretize(&ctxt), ctxt))
            } else {
                Ok((Type::Unit, ctxt))
            }
        }
    }
}

pub fn type_value<'a>(v: &'a Value, ctxt: TypeContext) -> TypeResult<'a, (Type, TypeContext)> {
    Ok(match v {
        Value::Nat(_) => (Type::Nat, ctxt),
        Value::Type(_) => (Type::Type, ctxt),
        Value::Sym(k) => (
            ctxt.get(k).ok_or(TypeError::UndefinedSymbol(k))?.concretize(&ctxt),
            ctxt,
        ),
        Value::Lam(ps, b) => {
            let mut ctxt = ctxt.clone();
            let mut expr_type = Vec::new(); // in reverse order!
            for p in ps.as_ref().get_symbols() {
                let (new, var) = ctxt.bind_to_tvar(p);
                ctxt = new;
                expr_type.push(Type::Var(var));
            }
            let (ctxt, ret_type_var) = ctxt.new_tvar();
            let ret_type_var = Type::Var(ret_type_var);
            let (ret_type, ctxt) = type_expr(b, ctxt)?;
            match ctxt.unify(&ret_type, &ret_type_var) {
                Err(UnifyErr::Inf) => return Err(TypeError::InfiniteType(ret_type, ret_type_var)),
                Err(UnifyErr::Mis) => todo!("Mismatch should be impossible with var & __"),
                Ok(ctxt) => {
                    // undoes reversal!
                    let lam_type = expr_type
                        .into_iter()
                        .fold(ret_type, |arr, curr| Type::fun(curr, arr));
                    (lam_type.concretize(&ctxt), ctxt)
                }
            }
        }
    })
}