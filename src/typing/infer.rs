use std::collections::{HashMap, HashSet};

use crate::{exprs::{Expr, Ident, ExprBody}, errors::{TypeResult, TypeError}, values::{VToken, Value}, parsing::sources::FilePos, stmts::Stmt, typing::{contraint::Constraint, NAT_TYPE, CHAR_TYPE}};

use super::{Type, scheme::Scheme, subst::{Substitutable, Subst}, contraint::solve, UNIT_TYPE};

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

type InferResult<'a, R> = TypeResult<'a, (Infer, R)>;

use crate::errors::TypeErrorBody::*;

#[derive(Debug)]
pub struct Infer {
    env: Env,
    var_count: usize,
}

impl Infer {
    pub fn new() -> Self {
        Self { 
            env: vec![(format!("+"), Scheme { forall: vec![], tipe: Type::fun(NAT_TYPE.clone(), Type::fun(NAT_TYPE.clone(), NAT_TYPE.clone())) })]
                    .into_iter()
                    .collect(), 
            var_count: 0 
        }
    }

    pub(crate) fn fresh(&mut self) -> usize {
        let out = self.var_count;
        self.var_count += 1;
        out
    }

    fn in_env(&mut self, name: String, sc: Scheme) {
        self.env.insert(name, sc);
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
    static ref NULL: Vec<Constraint<'static>> = Vec::with_capacity(0);
}

pub fn infer_top<'a>(infr: Infer, stmts: &'a Vec<Stmt<'a>>) -> InferResult<'a, Vec<Scheme>> {
    let mut infr = infr;
    let mut out = Vec::new();
    for s in stmts {
        match s {
            Stmt::Expr(e) => {
                let (new, sc) = infer_expr(infr, e)?;
                infr = new;
                out.push(sc);
            }
            Stmt::Bind(Ident { body: name, .. }, body) => {
                let (new, sc) = infer_expr(infr, body)?;
                infr = new;
                infr.in_env(name.clone(), sc.clone());
                out.push(sc)
            }
        }
    }
    Ok((infr, out))
}

fn infer_expr<'a>(infr: Infer, e: &'a Expr<'a>) -> InferResult<'a, Scheme> {
    let (mut infr, (t, cs)) = infer(infr, e)?;
    let sub = solve(cs)?;
    let sc = infr.close_over(t.apply(&sub));
    Ok((infr, sc))
}

fn infer<'a>(infr: Infer, e: &'a Expr<'a>) -> InferResult<'a, (Type, Vec<Constraint<'a>>)> {
    let Expr { pos, body } = e;    
    match body {
        ExprBody::Val(v) => {
            use Value::*;
            match v {
                Nat(_)  => Ok((infr, (NAT_TYPE.clone(), NULL.clone()))),
                Char(_) => Ok((infr, (CHAR_TYPE.clone(), NULL.clone()))),
                Sym(k) => 
                    infr.lookup_env(k, pos)
                        .map(|(i, t)| (i, (t, NULL.clone()))),
                Lam(x, e) => {
                    let name = match x.as_ref() {
                        Expr { body: ExprBody::Val(Sym(name)), .. } => 
                            name,
                        Expr{ pos, .. } =>
                            return Err(TypeError::new(pos.clone(), NotYetImplemented(format!("SExp lambda typing {:?}", body)))),
                    };
                    let mut infr = infr;
                    let tv = Type::Var(infr.fresh());
                    infr.in_env(name.clone(), Scheme { forall: vec![], tipe: tv.clone() });
                    println!("lam out {:?}", infr);
                    let (infr, (body_type, cs)) = infer(infr, e)?;
                    Ok((infr, (Type::fun(tv, body_type), cs)))
                }
            }
        }
        ExprBody::SExp(es) => {
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
            
            cs.push(Constraint { pos: pos.clone(), body: (f_type, full_f_type) });

            Ok((infr, (ret_type, cs)))
        },
    }
}