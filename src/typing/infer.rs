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
    static ref UNIT_TYPE: Type = Type::Data(String::from("Unit"), Vec::with_capacity(0));
    static ref NAT_TYPE: Type = Type::Data(String::from("Nat"), Vec::with_capacity(0));
    static ref CHAR_TYPE: Type = Type::Data(String::from("Char"), Vec::with_capacity(0));
}

#[allow(dead_code)]
pub(crate) fn infer_expr<'a>(infer: Infer, e: &'a Expr<'a>) -> InferResult<'a, (Type, Vec<Constraint>)> {
    match e {
        Expr::Val(v) => {
            use Value::*;
            let VToken { body, pos } = v;
            match body {
                Nat(_)  => Ok((infer, (NAT_TYPE.clone(), NULL.clone()))),
                Char(_) => Ok((infer, (CHAR_TYPE.clone(), NULL.clone()))),
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
        Expr::SExp(SToken { body: es, .. }) => {
            let mut es = es.into_iter();
            let f_expr = if let Some(fst) = es.next() {
                fst
            } else {
                return Ok((infer, (UNIT_TYPE.clone(), NULL.clone())));
            };
            let (mut infer, (f_type, mut cs)) = infer_expr(infer, f_expr)?;
            let mut arg_types = Vec::new();
            for e in es {
                let (new_infer, (b_type, b_cs)) = infer_expr(infer, e)?;
                infer = new_infer;
                cs.extend(b_cs.into_iter());
                arg_types.push(b_type);
            }
            let ret_type = Type::Var(infer.fresh());
            let full_f_type = 
                arg_types.into_iter()
                    .rev()
                    .fold(
                        ret_type.clone(), 
                        |prev, arg| Type::fun(arg, prev)
                    );
            
            cs.push((f_type, full_f_type));

            Ok((infer, (ret_type, cs)))
        },
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