use std::collections::{HashMap, HashSet};

use crate::{exprs::{Expr, Ident, ExprBody}, errors::{TypeResult, TypeError, TypeErrorBody::*}, values::Value, parsing::sources::FilePos, stmts::Stmt};

use super::{contraint::Constraint, Type, scheme::Scheme, subst::{Substitutable, Subst}, contraint::solve};

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

#[derive(Debug, Clone)]
pub struct Infer {
    env: Env,
    var_count: usize,
}

impl Infer {
    pub fn new() -> Self {
        Self { 
            env: vec![ (format!("+"), Scheme::concrete(Type::fun(Type::nat(), Type::fun(Type::nat(), Type::nat()))))
                     , (format!("chr"), Scheme::concrete(Type::fun(Type::nat(), Type::char())))
                     , (format!("ord"), Scheme::concrete(Type::fun(Type::char(), Type::nat())))
                     ]
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

    fn extend(&mut self, name: String, sc: Scheme) {
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

    fn in_env<'a, T, F>(mut self, name: &String, sc: &Scheme, f: F) -> InferResult<'a, T>
            where F: FnOnce(Self) -> InferResult<'a, T> {
        let mut infr = self.clone();
        infr.extend(name.clone(), sc.clone());
        let (infr, out) = f(infr)?;
        self.var_count = infr.var_count;
        Ok((self, out))
    }
}

fn null<'a>() -> Vec<Constraint<'a>> {
     Vec::with_capacity(0)
}


pub fn infer_mod<'a>(stmts: &'a Vec<Stmt<'a>>) -> TypeResult<'a, Vec<Scheme>> {
    let (_infr, scs) = infer_top(Infer::new(), stmts)?;
    Ok(scs)
}

pub fn infer_top<'a>(infr: Infer, stmts: &'a Vec<Stmt<'a>>) -> InferResult<'a, Vec<Scheme>> {
    let mut infr = infr;
    let mut out = Vec::new();
    for s in stmts {
        let (new, sc) = infer_stmt(infr, s)?;
        infr = new;
        out.push(sc);
    }
    Ok((infr, out))
}

fn infer_stmt<'a>(infr: Infer, s: &'a Stmt<'a>) -> InferResult<'a, Scheme> {
    let mut infr = infr;
    match s {
        Stmt::Expr(e) =>
            infer_expr(infr, e),
        Stmt::Bind(Ident { body: name, .. }, body) => {
            let ref sc = Scheme::concrete(Type::Var(infr.fresh()));
            let (mut new, sc) = infr.in_env(name, sc, |infr| infer_expr(infr, body))?;
            new.extend(name.clone(), sc.clone());
            Ok((new, sc))
        }
    }
}

fn infer_expr<'a>(infr: Infer, e: &'a Expr<'a>) -> InferResult<'a, Scheme> {
    let (mut infr, (t, cs)) = infer(infr, e)?;
    let sub = solve(cs)?;
    let sc = infr.close_over(t.apply(&sub));
    Ok((infr, sc))
}

fn infer<'a>(infr: Infer, Expr { pos, body }: &'a Expr<'a>) -> InferResult<'a, (Type, Vec<Constraint<'a>>)> {
    match body {
        ExprBody::Val(v) => {
            use Value::*;
            match v {
                Nat(_)  => Ok((infr, (Type::nat(), null()))),
                Char(_) => Ok((infr, (Type::char(), null()))),
                Sym(k) => 
                    infr.lookup_env(k, pos)
                        .map(|(i, t)| (i, (t, null()))),
                Lam(x, e) => {
                    let name = &x.body;
                    let mut infr = infr;
                    let tv = Type::Var(infr.fresh());
                    let ref sc = Scheme { forall: vec![], tipe: tv.clone() };
                    let (infr, (t, cs)) = 
                        infr.in_env(name, sc,
                            |infr| {
                                let (infr, (body_type, cs)) = 
                                    infer(infr, e)?;
                                Ok((infr, (Type::fun(tv, body_type), cs)))
                            }
                        )?;
                    // println!("lam out \n\tinfr: {:?}\n\tcs: {:?}\n\tt: {:?}", &infr.env, bodies(&cs), t);
                    Ok((infr, (t, cs)))
                }
            }
        }
        ExprBody::SExp(es) => {
            let mut es = es.into_iter();
            let fst = if let Some(fst) = es.next() {
                fst
            } else {
                return Ok((infr, (Type::unit(), null())));
            };
            
            let (mut infr, (mut last_t, mut cs)) = 
                infer(infr, fst)?;

            for e in es {
                let cnstr_pos = e.pos.clone();

                let (new_infr, (arg_t, new_cs)) = 
                    infer(infr, e)?;
                
                infr = new_infr;
                cs.extend(new_cs);
                
                let ret_type = Type::Var(infr.fresh());
                let body = (last_t, Type::fun(arg_t, ret_type.clone()));
                cs.push(Constraint { pos: cnstr_pos, body });
                
                last_t = ret_type;
            }

            // println!("sexp out \n\tinfr: {:?}\n\tcs: {:?}", &infr.env, bodies(&cs));
            Ok((infr, (last_t, cs)))
        },
    }
}