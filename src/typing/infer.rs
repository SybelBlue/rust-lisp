use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
    exprs::{Expr, Ident, ExprBody}, 
    errors::{TypeResult, TypeError, TypeErrorBody::{*, self}}, 
    values::Value, 
    parsing::sources::FilePos,
    stmts::Stmt,
    data::{DataDecl, Kind, Pattern, PatternBody, PartialKind},
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

    fn lookup_kind(&'a self, k: &'a String) -> Option<Kind<Type>> {
        self.ctxt.get_type(k).or_else(|| self.root.get_type(k)).cloned()
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

    fn insert_kind(mut self, Ident { body: k, pos }: Ident<'a>, v: Kind<Type>) -> Infer<'a, ()> {
        if let Some(v) = self.type_env.insert(k.clone(), pos.clone()) {
            self.err(pos.clone(), DuplicateNameAt(k, Some(v)))
        } else {
            self.ctxt.insert_type(k, v);
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

    fn locally_with<T, F, C>(mut self, mut idents: VecDeque<Ident<'a>>, f: F, c: &C) -> Infer<'a, T> 
            where 
                F: FnOnce(Self) -> Infer<'a, T>,
                C: Fn(Type, T) -> T {
        if idents.is_empty() {
            f(self)
        } else {
            let tv = self.fresh();
            let sc = Scheme::concrete(tv.clone());
            let ident = idents.pop_front().unwrap();
            let (new, prev) = 
                self.locally(
                    ident, 
                    sc,
                    |slf| slf.locally_with(idents, f, c)
                )?;
            Ok((new, c(tv, prev)))
        }
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
                let defed = match kind {
                    Kind::Type(()) =>
                        Kind::Type(Type::simple(&name.body)),
                    Kind::KFun(p, r) =>
                        Kind::KFun(p.clone(), r.clone()),
                };
                let (mut slf, ()) = self.insert_kind(name.clone(), defed)?;
                let prev_type_ctxt = slf.ctxt.types.clone();
                for (ctor, out) in ctors {
                    println!("{:?}", &slf.ctxt);
                    let (new, ret_type) = 
                        slf.define_kind(Kind::Type(()), out)?;
                    slf = new;
                    let mut ret_type = ret_type.as_type();
                    let (ctor_pos, ctor_name, type_args) = 
                        ctor.split_first();
                    if let Some(args) = type_args {
                        for arg_pat in args.into_iter().rev() {
                            let (new, arg_type) = 
                                slf.define_kind(Kind::Type(()), arg_pat)?;
                            slf = new;
                            ret_type = Type::fun(arg_type.as_type(), ret_type);
                        }
                    }

                    let (new, ()) = slf.insert_var(
                        &Ident { body: ctor_name.clone(), pos: ctor_pos.clone() },
                        Scheme::concrete(ret_type),
                        false
                    )?;
                    slf = new;
                }
                println!("{:?}", &slf.ctxt);
                slf.ctxt.types = prev_type_ctxt;
                Ok((slf, Scheme::concrete(Type::unit())))
            }
        }
    }

    fn define_kind(mut self, mut target_kind: Kind<()>, p: &'a Pattern<'a>) -> Infer<'a, PartialKind<'a>> {
        let (pos, fst, op_rst) = p.split_first();
        let (mut slf, kind) = 
            if let Some(k) = self.lookup_kind(fst) {
                (self, k)
            } else {
                let t = if target_kind == Kind::Type(()) {
                    if op_rst.is_none() {
                        self.fresh()
                    } else {
                        return self.err(pos.clone(), TooManyArgs);
                    }
                } else {
                    Type::Data(fst.clone(), Vec::new())
                };
                let sc = Scheme::concrete(t.clone());
                let fst_ident = Ident { body: fst.clone(), pos: pos.clone() };
                let (slf, ()) = self.insert_var(&fst_ident, sc, false)?;
                return Ok((slf, PartialKind::Finished(t)));
            };
        let mut ks = Vec::new();
        if let Some(rst) = op_rst {
            let mut acc = if target_kind == Kind::Type(()) { None } else { Some(target_kind) };
            for r in rst {
                let (new, t) = match acc {
                    None =>
                        return slf.err(r.pos.clone(), TooManyArgs),
                    Some(Kind::KFun(k, nxt)) => {
                        acc = Some(*nxt);
                        slf.define_kind(*k, r)?
                    }
                    Some(Kind::Type(())) => {
                        acc = None;
                        slf.define_kind(Kind::Type(()), r)?
                    }
                };
                ks.push(t);
                slf = new;
            }
            target_kind = acc.unwrap_or(Kind::Type(()));
        }

        if target_kind.matches(&kind) {
            if target_kind == Kind::Type(()) {
                if let Kind::Type(t) = kind {
                    Ok((slf, PartialKind::Finished(t)))
                } else {
                    slf.err(pos.clone(), ExpectedTypeGotKind { kind })
                }
            } else {
                Ok((slf, PartialKind::Partial(fst, ks, target_kind)))
            }
        } else {
            slf.err(pos.clone(), KindMismatch { got: kind, expected: target_kind })
        }
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

    fn value_constraints(self, pos: &'a FilePos<'a>, v: &'a Value<'a>) -> Infer<'a, TContraints<'a>> {
        use Value::*;
        match v {
            Nat(_)  => Ok((self, null(Type::nat()))),
            Char(_) => Ok((self, null(Type::char()))),
            Sym(k) => 
                self.lookup_var(k, pos)
                    .map(|(i, t)| (i, null(t))),
            Lam(x, e) => {
                match flatten_args(x.clone()) {
                    Ok(args) => 
                        self.locally_with(
                            VecDeque::from(args),
                            |slf| 
                                slf.constraints(e),
                            &|tv, (t, cs)|
                                (Type::fun(tv, t), cs),
                        ),
                    Err(e) =>
                        self.err(e.pos, e.body),
                }
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

fn flatten_args<'a>(Pattern { pos, body }: Pattern<'a>) -> TypeResult<'a, Vec<Ident<'a>>> {
    match body {
        PatternBody::PSym(body) => 
            Ok(vec![Ident { pos, body }]),
        PatternBody::PSExp(fst, rst) => {
            let mut args = vec![fst];
            for Pattern { pos, body } in rst {
                match body {
                    PatternBody::PSym(body) => 
                        args.push(Ident { body, pos }),
                    PatternBody::PSExp(_, _) => 
                        return Err(TypeError::new(pos, NotYetImplemented(format!("Data Pattern Matching")))),
                }
            }
            Ok(args)
        }
    }
}
