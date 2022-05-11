use std::collections::{HashMap, HashSet};

use crate::{exprs::Expr, errors::{TypeResult, TypeError}, values::{VToken, Value}, parsing::sources::FilePos};

use super::{Type, Scheme, subst::{Substitutable, Subst}};

type Constraint = (Type, Type);

type Env = HashMap<String, Scheme>;

impl Substitutable for Env {
    fn apply(&self, sub: &Subst) -> Self {
        self.into_iter()
            .map(|(k, v)| (k.clone(), v.apply(sub)))
            .collect()
    }

    fn ftv(&self, used: &mut HashSet<usize>) {
        self.values().for_each(|s| s.ftv(used));
    }
}

pub(crate) type InferResult<'a, R> = TypeResult<'a, (Infer, R)>;

use crate::errors::TypeErrorBody::*;

pub(crate) struct Infer {
    env: Env,
    var_count: usize,
}

impl Infer {
    pub(crate) fn fresh(&mut self) -> usize {
        let out = self.var_count;
        self.var_count += 1;
        out
    }
}

lazy_static::lazy_static! {
    static ref NULL: Vec<Constraint> = Vec::with_capacity(0);
    static ref NAT_TYPE: Type = Type::Data(String::from("Nat"), Vec::with_capacity(0));
    static ref CHR_TYPE: Type = Type::Data(String::from("Char"), Vec::with_capacity(0));
}

#[allow(dead_code)]
pub(crate) fn infer<'a>(infer: Infer, e: &'a Expr<'a>) -> InferResult<'a, (Type, Vec<Constraint>)> {
    match e {
        Expr::Val(v) => {
            use Value::*;
            let VToken { body, pos } = v;
            match body {
                Nat(_)  => Ok((infer, (NAT_TYPE.clone(), NULL.clone()))),
                Char(_) => Ok((infer, (CHR_TYPE.clone(), NULL.clone()))),
                Sym(k) => 
                    lookup_env(infer, k, pos)
                    .map(|(i, t)| (i, (t, NULL.clone()))),
                Lam(_, _) => todo!(),
            }
        }
        Expr::SExp(_) => todo!(),
    }
}

fn lookup_env<'a>(infer: Infer, k: &'a String, pos: &'a FilePos<'a>) -> InferResult<'a, Type> {
    if let Some(s) = infer.env.get(k).cloned() {
        let mut infer = infer;
        let t = s.instantiate(&mut infer);
        Ok((infer, t))
    } else {
        Err(TypeError::new(pos.clone(), UndefinedSymbol(k)))
    }
}