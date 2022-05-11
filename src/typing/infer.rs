use std::collections::{HashMap, HashSet};

use crate::{exprs::{Expr, SToken}, errors::{TypeResult, TypeError}, values::{VToken, Value}, parsing::sources::FilePos};

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

    pub(crate) fn insert(&mut self, name: String, sc: Scheme) {
        self.env.entry(name)
            .and_modify(|s| *s = sc);
    }
}

lazy_static::lazy_static! {
    static ref NULL: Vec<Constraint> = Vec::with_capacity(0);
    static ref NAT_TYPE: Type = Type::Data(String::from("Nat"), Vec::with_capacity(0));
    static ref CHR_TYPE: Type = Type::Data(String::from("Char"), Vec::with_capacity(0));
}

#[allow(dead_code)]
pub(crate) fn infer_expr<'a>(infer: Infer, e: &'a Expr<'a>) -> InferResult<'a, (Type, Vec<Constraint>)> {
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
                Lam(x, e) => {
                    let name = match x.as_ref() {
                        Expr::Val(VToken { body: Sym(name), .. }) => name,
                        body@Expr::Val(VToken { pos, .. }) |
                            body@Expr::SExp(SToken { pos, .. }) => 
                                return Err(TypeError::new(pos.clone(), NotYetImplemented(format!("SExp lambda typing {:?}", body)))),
                    };
                    let mut infer = infer;
                    let tv = Type::Var(infer.fresh());
                    let (mut infer, (body_type, cs)) = infer_expr(infer, e)?;
                    infer.insert(name.clone(), Scheme { forall: vec![], tipe: tv.clone() });
                    Ok((infer, (Type::fun(tv, body_type), cs)))
                }
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