use std::collections::{HashMap, HashSet};

use crate::{
    exprs::{Expr, Ident, ExprBody}, 
    errors::{TypeResult, TypeError, TypeErrorBody::{*, self}}, 
    values::Value, 
    parsing::sources::FilePos,
    stmts::Stmt,
    data::{DataDecl, Kind},
};

use super::{
    Type, 
    scheme::Scheme, 
    subst::Substitutable, 
    contexts::Context,
    contraint::{Constraint, solve}
};

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
   let (ctxt, res) = ctxt.finish();
   (ctxt, res.map(|()| out))
}


type TContraints<'a> = (Type, Vec<Constraint<'a>>);

#[inline]
fn null<'a>(t: Type) -> TContraints<'a> {
    (t, Vec::with_capacity(0))
}

type Infer<'a, R> = Result<(InferContext<'a>, R), (Context, TypeError<'a>)>;

#[derive(Debug, Clone)]
struct InferContext<'a> {
    root: Context,
    ctxt: Context,
    var_env: HashMap<String, FilePos<'a>>,
    type_env: HashMap<String, FilePos<'a>>,
    var_count: usize,
}

impl<'a> InferContext<'a> {
    fn new(ctxt: Context) -> Self {
        Self { 
            root: ctxt,
            ctxt: Context::blank(),
            var_env: HashMap::new(), 
            type_env: HashMap::new(),
            var_count: 0 
        }
    }

    fn fresh(&mut self) -> Type {
        let out = self.var_count;
        self.var_count += 1;
        Type::Var(out)
    }

    fn err<T>(self, pos: FilePos<'a>, body: TypeErrorBody<'a>) -> Infer<'a, T> {
        Err((self.root, TypeError::new(pos, body)))
    }

    fn finish(self) -> (Context, TypeResult<'a, ()>) {
        let Self { root: mut ctxt, ctxt: temp, var_env: env, .. } = self;
        for (k, l) in env {
            if ctxt.contains_var(&k) {
                return (ctxt, Err(TypeError::new(l, DuplicateNameAt(k.clone(), None))));
            }
        }
        ctxt.extend(temp);
        (ctxt, Ok(()))
    }

    fn lookup_var(mut self, k: &'a String, pos: &'a FilePos<'a>) -> Infer<'a, Type> {
        if let Some(s) = self.ctxt.get_var(k).or_else(|| self.root.get_var(k)).cloned() {
            let t = s.instantiate(&mut || self.fresh());
            Ok((self, t))
        } else {
            self.err(pos.clone(), UndefinedSymbol(k))
        }
    }

    fn lookup_type(mut self, key: &'a String, pos: &'a FilePos<'a>) -> Infer<'a, Type> {
        if let Some(kind) = self.ctxt.get_type(key).or_else(|| self.root.get_type(key)) {
            if kind == &Kind::Type {
                Ok((self, Type::Data(key.clone(), Vec::with_capacity(0))))
            } else {
                let kind = kind.clone();
                self.err(pos.clone(), ExpectedTypeGotKind { kind, name: key.clone() })
            }
        } else {
            let tv = self.fresh();
            Ok((self, tv))
        }
    }

    fn lookup_kind(self, k: &'a String, pos: &'a FilePos<'a>) -> Infer<'a, Kind> {
        if let Some(k) = self.ctxt.get_type(k).or_else(|| self.root.get_type(k)).cloned() {
            Ok((self, k))
        } else {
            self.err(pos.clone(), UndefinedSymbol(k))
        }
    }

    fn insert_var(mut self, Ident { body: k, pos }: &Ident<'a>, sc: Scheme, rewrite_ok: bool) -> Infer<'a, ()> {
        if let Some(v) = self.var_env.insert(k.clone(), pos.clone()) {
            if !rewrite_ok {
                return self.err(pos.clone(), 
                        DuplicateNameAt(k.clone(), Some(v)));
            }
        }
        self.ctxt.insert_var(k.clone(), sc);
        Ok((self, ()))
    }

    fn insert_type(mut self, Ident { body: k, pos }: &Ident<'a>, v: Kind) -> Infer<'a, ()> {
        if let Some(v) = self.type_env.insert(k.clone(), pos.clone()) {
            self.err(pos.clone(), DuplicateNameAt(k.clone(), Some(v)))
        } else {
            self.ctxt.insert_type(k.clone(), v);
            Ok((self, ()))
        }
    }

    fn generalize(&mut self, tipe: Type) -> Scheme {
        let mut used = HashSet::new();
        tipe.ftv(&mut used);
        let mut defined = HashSet::new();
        self.ctxt.get_vartypes().for_each(|s| s.ftv(&mut defined));
        Scheme { 
            forall: used.difference(&defined).map(|x| *x).collect(), 
            tipe 
        }
    }

    fn close_over(&mut self, t: Type) -> Scheme {
        self.generalize(t).normalize()
    }

    fn locally<T, F>(mut self, ident: Ident<'a>, sc: Scheme, f: F) -> Infer<'a, T>
            where F: FnOnce(Self) -> Infer<'a, T> {
        let slf = self.clone();
        let (slf, ()) = slf.insert_var(&ident, sc, true)?;
        let (slf, out) = f(slf)?;
        self.var_count = slf.var_count;
        Ok((self, out))
    }

    fn infer_stmt(mut self, s: &'a Stmt<'a>) -> Infer<'a, Scheme> {
        match s {
            Stmt::Expr(e) =>
                self.infer_expr(e),
            Stmt::Bind(ident, body) => {
                let sc = Scheme::concrete(self.fresh());
                let (new, sc) = self.locally(ident.clone(), sc.clone(), |slf| slf.infer_expr(body))?;
                let (new, ()) = new.insert_var(&ident, sc.clone(), false)?;
                Ok((new, sc))
            }
            Stmt::Data(DataDecl { name, kind, ctors }) => {
                let (mut slf, ()) = self.insert_type(name, kind.clone())?;
                for (cname, e) in ctors {
                    let (mut new, t) = slf.read_type(e)?;
                    let sc = new.close_over(t);
                    let (new, ()) = new.insert_var(cname, sc, false)?;
                    slf = new;
                }
                println!("{:?}", &slf.ctxt);
                Ok((slf, Scheme::concrete(Type::unit())))
            }
        }
    }

    fn read_type(self, e: &'a Expr<'a>) -> Infer<'a, Type> {
        todo!("read type")
    }
    
    fn infer_expr(self, e: &'a Expr<'a>) -> Infer<'a, Scheme> {
        let (mut slf, (t, cs)) = self.constraints(e)?;
        match solve(cs) {
            Ok(sub) => {
                let sc = slf.close_over(t.apply(&sub));
                Ok((slf, sc))
            }
            Err(x) =>
                Err((slf.root, x))
        }
    }

    fn constraints(self, Expr { pos, body }: &'a Expr<'a>) -> Infer<'a, TContraints<'a>> {
        match body {
            ExprBody::Val(v) => 
                self.value_constraints(pos, v),
            ExprBody::SExp(es) => 
                self.sexp_constraints(es),
        }
    }

    fn value_constraints(mut self, pos: &'a FilePos<'a>, v: &'a Value<'a>) -> Infer<'a, TContraints<'a>> {
        use Value::*;
        match v {
            Nat(_)  => Ok((self, null(Type::nat()))),
            Char(_) => Ok((self, null(Type::char()))),
            Sym(k) => 
                self.lookup_var(k, pos)
                    .map(|(i, t)| (i, null(t))),
            Lam(x, e) => {
                let tv = self.fresh();
                let sc = Scheme { forall: vec![], tipe: tv.clone() };
                let (slf, (t, cs)) = 
                    self.locally(x.clone(), sc,
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
    
    fn sexp_constraints(self, es: &'a Vec<Expr<'a>>) -> Infer<'a, TContraints<'a>> {
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
