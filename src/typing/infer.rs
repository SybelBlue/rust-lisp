use std::collections::{HashMap, HashSet};

use crate::{exprs::{Expr, Ident, ExprBody}, errors::{TypeResult, TypeError, TypeErrorBody::*}, values::Value, parsing::sources::FilePos, stmts::Stmt};

use super::{contraint::Constraint, Type, scheme::Scheme, subst::{Substitutable, Subst}, contraint::solve, contexts::Context};

fn null<'a>() -> Vec<Constraint<'a>> {
    Vec::with_capacity(0)
}

pub fn infer<'a>(ctxt: Context, stmts: &'a Vec<Stmt<'a>>) ->  (Context, TypeResult<'a, Vec<Scheme>>) {
   let mut infr = Infer::new(ctxt);
   let mut out = Vec::new();
   for s in stmts {
       match infr.stmt(s) {
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

type Env = HashMap<String, Scheme>;
type TContraints<'a> = (Type, Vec<Constraint<'a>>);

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

    fn stmt<'a>(mut self, s: &'a Stmt<'a>) -> InferResult<'a, Scheme> {
        match s {
            Stmt::Expr(e) =>
                self.expr(e),
            Stmt::Bind(Ident { body: name, .. }, body) => {
                let ref sc = Scheme::concrete(self.fresh());
                let (mut new, sc) = self.locally(name, sc, |infr| infr.expr(body))?;
                new.env.insert(name.clone(), sc.clone());
                Ok((new, sc))
            }
        }
    }
    
    fn expr<'a>(self, e: &'a Expr<'a>) -> InferResult<'a, Scheme> {
        let (mut infr, (t, cs)) = self.constraints(e)?;
        match solve(cs) {
            Ok(sub) => {
                let sc = infr.close_over(t.apply(&sub));
                Ok((infr, sc))
            }
            Err(x) =>
                Err((infr.ctxt, x))
        }
    }

    fn constraints<'a>(self, Expr { pos, body }: &'a Expr<'a>) -> InferResult<'a, TContraints<'a>> {
        match body {
            ExprBody::Val(v) => 
                self.value_constraints(pos, v),
            ExprBody::SExp(es) => 
                self.sexp_constraints(es),
        }
    }

    fn value_constraints<'a>(mut self, pos: &'a FilePos<'a>, v: &'a Value<'a>) -> InferResult<'a, TContraints<'a>> {
        use Value::*;
        match v {
            Nat(_)  => Ok((self, (Type::nat(), null()))),
            Char(_) => Ok((self, (Type::char(), null()))),
            Sym(k) => 
                self.lookup_env(k, pos)
                    .map(|(i, t)| (i, (t, null()))),
            Lam(x, e) => {
                let name = &x.body;
                let tv = self.fresh();
                let ref sc = Scheme { forall: vec![], tipe: tv.clone() };
                let (infr, (t, cs)) = 
                    self.locally(name, sc,
                        |infr| {
                            let (infr, (body_type, cs)) = 
                                infr.constraints(e)?;
                            Ok((infr, (Type::fun(tv, body_type), cs)))
                        }
                    )?;
                Ok((infr, (t, cs)))
            }
        }
    }
    
    fn sexp_constraints<'a>(self, es: &'a Vec<Expr<'a>>) -> InferResult<'a, TContraints<'a>> {
        let mut es = es.into_iter();
        let fst = if let Some(fst) = es.next() {
            fst
        } else {
            return Ok((self, (Type::unit(), null())));
        };
        
        let (mut infr, (mut last_t, mut cs)) = 
            self.constraints(fst)?;

        for e in es {
            let cnstr_pos = e.pos.clone();

            let (new_infr, (arg_t, new_cs)) = 
                infr.constraints(e)?;
            
            infr = new_infr;
            cs.extend(new_cs);
            
            let ret_type = infr.fresh();
            let body = (Type::fun(arg_t, ret_type.clone()), last_t);
            cs.push(Constraint { pos: cnstr_pos, body });
            
            last_t = ret_type;
        }

        Ok((infr, (last_t, cs)))
    }
}
