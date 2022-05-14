use std::collections::{HashMap, HashSet};

use crate::{exprs::{Expr, Ident, ExprBody}, errors::{TypeResult, TypeError, TypeErrorBody::*}, values::Value, parsing::sources::FilePos, stmts::Stmt};

use super::{contraint::Constraint, Type, scheme::Scheme, subst::{Substitutable, Subst}, contraint::solve, contexts::Context};

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

type InferResult<'a, R> = Result<(Infer, R), (Context, TypeError<'a>)>;

#[derive(Debug, Clone)]
struct Infer {
    ctxt: Context,
    env: Env,
    var_count: usize,
}

impl Infer {
    fn new(ctxt: Context) -> Self {
        Self { 
            ctxt,
            env: Env::new(), 
            var_count: 0 
        }
    }

    fn fresh(&mut self) -> Type {
        let out = self.var_count;
        self.var_count += 1;
        Type::Var(out)
    }

    fn insert(&mut self, name: String, sc: Scheme) {
        self.env.insert(name, sc);
    }

    fn finish(self) -> Context {
        let Self { mut ctxt, env, .. } = self;
        env.into_iter().for_each(|(k, v)| ctxt.insert(k, v));
        ctxt
    }

    fn lookup_env<'a>(mut self, k: &'a String, pos: &'a FilePos<'a>) -> InferResult<'a, Type> {
        if let Some(s) = self.env.get(k).or_else(|| self.ctxt.get(k)).cloned() {
            let t = s.instantiate(&mut || self.fresh());
            Ok((self, t))
        } else {
            Err((self.ctxt, TypeError::new(pos.clone(), UndefinedSymbol(k))))
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

    fn locally<'a, T, F>(mut self, name: &String, sc: &Scheme, f: F) -> InferResult<'a, T>
            where F: FnOnce(Self) -> InferResult<'a, T> {
        let mut infr = self.clone();
        infr.env.insert(name.clone(), sc.clone());
        let (infr, out) = f(infr)?;
        self.var_count = infr.var_count;
        Ok((self, out))
    }
}

fn null<'a>() -> Vec<Constraint<'a>> {
     Vec::with_capacity(0)
}


pub fn infer_mod<'a>(stmts: &'a Vec<Stmt<'a>>) -> (Context, TypeResult<'a, Vec<Scheme>>) {
    infer_top(Context::new(), stmts)
}

pub fn infer_top<'a>(ctxt: Context, stmts: &'a Vec<Stmt<'a>>) ->  (Context, TypeResult<'a, Vec<Scheme>>) {
    let mut infr = Infer::new(ctxt);
    let mut out = Vec::new();
    for s in stmts {
        match infer_stmt(infr, s) {
            Ok((new, sc)) => {
                infr = new;
                out.push(sc);
            }
            Err((ctxt, te)) =>
                return (ctxt, Err(te)),
        }
    }
    (infr.finish(), Ok(out))
}

fn infer_stmt<'a>(infr: Infer, s: &'a Stmt<'a>) -> InferResult<'a, Scheme> {
    let mut infr = infr;
    match s {
        Stmt::Expr(e) =>
            infer_expr(infr, e),
        Stmt::Bind(Ident { body: name, .. }, body) => {
            let ref sc = Scheme::concrete(infr.fresh());
            let (mut new, sc) = infr.locally(name, sc, |infr| infer_expr(infr, body))?;
            new.insert(name.clone(), sc.clone());
            Ok((new, sc))
        }
    }
}

fn infer_expr<'a>(infr: Infer, e: &'a Expr<'a>) -> InferResult<'a, Scheme> {
    let (mut infr, (t, cs)) = infer(infr, e)?;
    match solve(cs) {
        Ok(sub) => {
            let sc = infr.close_over(t.apply(&sub));
            Ok((infr, sc))
        }
        Err(x) =>
            Err((infr.ctxt, x))
    }
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
                    let tv = infr.fresh();
                    let ref sc = Scheme { forall: vec![], tipe: tv.clone() };
                    let (infr, (t, cs)) = 
                        infr.locally(name, sc,
                            |infr| {
                                let (infr, (body_type, cs)) = 
                                    infer(infr, e)?;
                                Ok((infr, (Type::fun(tv, body_type), cs)))
                            }
                        )?;
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
                
                let ret_type = infr.fresh();
                let body = (Type::fun(arg_t, ret_type.clone()), last_t);
                cs.push(Constraint { pos: cnstr_pos, body });
                
                last_t = ret_type;
            }

            Ok((infr, (last_t, cs)))
        },
    }
}