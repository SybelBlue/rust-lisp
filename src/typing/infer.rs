use std::collections::{HashMap, HashSet};

use crate::{exprs::{Expr, SToken}, errors::{TypeResult, TypeError}, values::{VToken, Value}, parsing::sources::FilePos, stmts::Stmt, typing::contraint::Constraint};

use super::{Type, scheme::Scheme, subst::{Substitutable, Subst}, contraint::solve};

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

    fn insert(&mut self, name: String, sc: Scheme) {
        self.env.entry(name)
            .and_modify(|s| *s = sc);
    }

    fn lookup_env<'a>(mut self, k: &'a String, pos: &'a FilePos<'a>) -> InferResult<'a, Type> {
        if let Some(s) = self.env.get(k).cloned() {
            let t = s.instantiate(&mut self);
            Ok((self, t))
        } else {
            Err(TypeError::new(pos.clone(), UndefinedSymbol(k)))
        }
    }

    fn generalize(&mut self, tipe: Type) -> Scheme {
        let mut used = HashSet::new();
        tipe.ftv(&mut used);
        let mut defined = HashSet::new();
        self.env.ftv(&mut defined);
        Scheme { 
            forall: used.difference(&defined).map(|x| *x).collect(), 
            tipe 
        }
    }

    fn close_over(&mut self, t: Type) -> Scheme {
        self.generalize(t).normalize()
    }
}

lazy_static::lazy_static! {
    static ref NULL: Vec<Constraint> = Vec::with_capacity(0);
    static ref UNIT_TYPE: Type = Type::Data(String::from("Unit"), Vec::with_capacity(0));
    static ref NAT_TYPE: Type = Type::Data(String::from("Nat"), Vec::with_capacity(0));
    static ref CHAR_TYPE: Type = Type::Data(String::from("Char"), Vec::with_capacity(0));
}

#[allow(dead_code)]
pub(crate) fn infer_top<'a>(infr: Infer, stmts: Vec<&'a Stmt<'a>>) -> InferResult<'a, ()> {
    let mut infr = infr;
    for s in stmts {
        match s {
            Stmt::Expr(e) => {
                let (new, _) = infer(infr, e)?;
                infr = new;
            }
            Stmt::Bind(ident, body) => {
                let (new, _) = infer_expr(infr, body)?;
                infr = new;
                todo!()
            }
        }
    }
    Ok((infr, ()))
}

fn infer_expr<'a>(infr: Infer, e: &'a Expr<'a>) -> InferResult<'a, Scheme> {
    let (mut infr, (t, cs)) = infer(infr, e)?;
    let sub = solve(cs)?;
    let sc = infr.close_over(t.apply(&sub));
    Ok((infr, sc))
}

fn infer<'a>(infr: Infer, e: &'a Expr<'a>) -> InferResult<'a, (Type, Vec<Constraint>)> {
    match e {
        Expr::Val(v) => {
            use Value::*;
            let VToken { body, pos } = v;
            match body {
                Nat(_)  => Ok((infr, (NAT_TYPE.clone(), NULL.clone()))),
                Char(_) => Ok((infr, (CHAR_TYPE.clone(), NULL.clone()))),
                Sym(k) => 
                    infr.lookup_env(k, pos)
                        .map(|(i, t)| (i, (t, NULL.clone()))),
                Lam(x, e) => {
                    let name = match x.as_ref() {
                        Expr::Val(VToken { body: Sym(name), .. }) => name,
                        body@Expr::Val(VToken { pos, .. }) |
                            body@Expr::SExp(SToken { pos, .. }) => 
                                return Err(TypeError::new(pos.clone(), NotYetImplemented(format!("SExp lambda typing {:?}", body)))),
                    };
                    let mut infr = infr;
                    let tv = Type::Var(infr.fresh());
                    let (mut infr, (body_type, cs)) = infer(infr, e)?;
                    infr.insert(name.clone(), Scheme { forall: vec![], tipe: tv.clone() });
                    Ok((infr, (Type::fun(tv, body_type), cs)))
                }
            }
        }
        Expr::SExp(SToken { body: es, .. }) => {
            let mut es = es.into_iter();
            let f_expr = if let Some(fst) = es.next() {
                fst
            } else {
                return Ok((infr, (UNIT_TYPE.clone(), NULL.clone())));
            };
            let (mut infr, (f_type, mut cs)) = infer(infr, f_expr)?;
            let mut arg_types = Vec::new();
            for e in es {
                let (new_infer, (b_type, b_cs)) = infer(infr, e)?;
                infr = new_infer;
                cs.extend(b_cs.into_iter());
                arg_types.push(b_type);
            }
            let ret_type = Type::Var(infr.fresh());
            let full_f_type = 
                arg_types.into_iter()
                    .rev()
                    .fold(
                        ret_type.clone(), 
                        |prev, arg| Type::fun(arg, prev)
                    );
            
            cs.push((f_type, full_f_type));

            Ok((infr, (ret_type, cs)))
        },
    }
}