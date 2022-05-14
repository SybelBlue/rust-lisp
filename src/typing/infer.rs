use std::collections::{HashMap, HashSet};

use crate::{
    exprs::{Expr, Ident, ExprBody}, 
    errors::{TypeResult, TypeError, TypeErrorBody::*}, 
    values::Value, 
    parsing::sources::FilePos, 
    stmts::Stmt
};

use super::{contraint::Constraint, Type, scheme::Scheme, subst::Substitutable, contraint::solve, contexts::Context};

pub fn infer<'a>(ctxt: Context, stmts: &'a Vec<Stmt<'a>>) -> (Context, TypeResult<'a, Vec<Scheme>>) {
   let mut ctxt = InferContext::new(ctxt);
   let mut out = Vec::new();
   for s in stmts {
       match ctxt.infer_stmt(s) {
           Ok((new, sc)) => {
               ctxt = new;
               out.push(sc);
           }
           Err((ctxt, te)) =>
               return (ctxt, Err(te)),
       }
   }
   (ctxt.finish(), Ok(out))
}


type TContraints<'a> = (Type, Vec<Constraint<'a>>);

#[inline]
fn null<'a>(t: Type) -> TContraints<'a> {
    (t, Vec::with_capacity(0))
}

type Infer<'a, R> = Result<(InferContext, R), (Context, TypeError<'a>)>;

#[derive(Debug, Clone)]
struct InferContext {
    ctxt: Context,
    env: HashMap<String, Scheme>,
    var_count: usize,
}

impl InferContext {
    fn new(ctxt: Context) -> Self {
        Self { 
            ctxt,
            env: HashMap::new(), 
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

    fn lookup_env<'a>(mut self, k: &'a String, pos: &'a FilePos<'a>) -> Infer<'a, Type> {
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
        self.env.values().for_each(|s| s.ftv(&mut defined));
        Scheme { 
            forall: used.difference(&defined).map(|x| *x).collect(), 
            tipe 
        }
    }

    fn close_over(&mut self, t: Type) -> Scheme {
        self.generalize(t).normalize()
    }

    fn locally<'a, T, F>(mut self, name: &String, sc: &Scheme, f: F) -> Infer<'a, T>
            where F: FnOnce(Self) -> Infer<'a, T> {
        let mut slf = self.clone();
        slf.env.insert(name.clone(), sc.clone());
        let (slf, out) = f(slf)?;
        self.var_count = slf.var_count;
        Ok((self, out))
    }

    fn infer_stmt<'a>(mut self, s: &'a Stmt<'a>) -> Infer<'a, Scheme> {
        match s {
            Stmt::Expr(e) =>
                self.infer_expr(e),
            Stmt::Bind(Ident { body: name, .. }, body) => {
                let ref sc = Scheme::concrete(self.fresh());
                let (mut new, sc) = self.locally(name, sc, |slf| slf.infer_expr(body))?;
                new.env.insert(name.clone(), sc.clone());
                Ok((new, sc))
            }
        }
    }
    
    fn infer_expr<'a>(self, e: &'a Expr<'a>) -> Infer<'a, Scheme> {
        let (mut slf, (t, cs)) = self.constraints(e)?;
        match solve(cs) {
            Ok(sub) => {
                let sc = slf.close_over(t.apply(&sub));
                Ok((slf, sc))
            }
            Err(x) =>
                Err((slf.ctxt, x))
        }
    }

    fn constraints<'a>(self, Expr { pos, body }: &'a Expr<'a>) -> Infer<'a, TContraints<'a>> {
        match body {
            ExprBody::Val(v) => 
                self.value_constraints(pos, v),
            ExprBody::SExp(es) => 
                self.sexp_constraints(es),
        }
    }

    fn value_constraints<'a>(mut self, pos: &'a FilePos<'a>, v: &'a Value<'a>) -> Infer<'a, TContraints<'a>> {
        use Value::*;
        match v {
            Nat(_)  => Ok((self, null(Type::nat()))),
            Char(_) => Ok((self, null(Type::char()))),
            Sym(k) => 
                self.lookup_env(k, pos)
                    .map(|(i, t)| (i, null(t))),
            Lam(x, e) => {
                let name = &x.body;
                let tv = self.fresh();
                let ref sc = Scheme { forall: vec![], tipe: tv.clone() };
                let (slf, (t, cs)) = 
                    self.locally(name, sc,
                        |slf| {
                            let (slf, (body_type, cs)) = 
                                slf.constraints(e)?;
                            Ok((slf, (Type::fun(tv, body_type), cs)))
                        }
                    )?;
                Ok((slf, (t, cs)))
            }
        }
    }
    
    fn sexp_constraints<'a>(self, es: &'a Vec<Expr<'a>>) -> Infer<'a, TContraints<'a>> {
        let mut es = es.into_iter();
        let fst = if let Some(fst) = es.next() {
            fst
        } else {
            return Ok((self, null(Type::unit())));
        };
        
        let (mut slf, (mut last_t, mut cs)) = 
            self.constraints(fst)?;

        for e in es {
            let cnstr_pos = e.pos.clone();

            let (new_slf, (arg_t, new_cs)) = 
                slf.constraints(e)?;
            
            slf = new_slf;
            cs.extend(new_cs);
            
            let ret_type = slf.fresh();
            let body = (Type::fun(arg_t, ret_type.clone()), last_t);
            cs.push(Constraint { pos: cnstr_pos, body });
            
            last_t = ret_type;
        }

        Ok((slf, (last_t, cs)))
    }
}
