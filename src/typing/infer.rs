use std::{collections::{HashMap, HashSet, VecDeque}, slice::Iter};

use crate::{
    exprs::{Expr, Ident, ExprBody}, 
    errors::{TypeResult, TypeError, TypeErrorBody::{*, self}}, 
    values::Value, 
    parsing::sources::FilePos,
    stmts::Stmt,
    data::{Kind, Data, DataBody},
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

    fn lookup_kind(&'a self, k: &'a String) -> Option<Kind> {
        self.ctxt.get_type(k).or_else(|| self.root.get_type(k)).cloned()
    }

    fn insert_var(mut self, k: String, pos: FilePos<'a>, sc: Scheme, rewrite_ok: bool) -> Infer<'a, ()> {
        if let Some(v) = self.var_env.insert(k.clone(), pos.clone()) {
            if !rewrite_ok {
                return self.err(pos, 
                        DuplicateNameAt(k, Some(v)));
            }
        }
        self.ctxt.insert_var(k, sc);
        Ok((self, ()))
    }

    fn insert_kind(mut self, Ident { body: k, pos }: Ident<'a>, v: Kind) -> Infer<'a, ()> {
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
        let (slf, ()) = slf.insert_var(ident.body.clone(), ident.pos.clone(), sc, true)?;
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
                let (new, ()) = new.insert_var(ident.body.clone(), ident.pos.clone(), sc.clone(), false)?;
                Ok((new, sc))
            }
            Stmt::Decl(decl) => {
                let (mut slf, ()) = self.insert_kind(decl.name.clone(), decl.kind.clone())?;
                println!("{:?}", slf.ctxt.types);
                for (cbody, cout) in &decl.ctors {
                    let mut type_vars = HashMap::new();
                    let (new, mut ret_type) = 
                        slf.define_kind(cout, Kind::Type, &mut type_vars)?;
                    slf = new;
                    let (nm_pos, nm, op_rst) = cbody.split_first();
                    if let Some(rst) = op_rst {
                        for arg in rst.into_iter().rev() {
                            let (new, arg_type) = 
                                slf.define_kind(arg, Kind::Type, &mut type_vars)?;
                            slf = new;
                            ret_type = Type::fun(arg_type, ret_type);
                            println!("\targs {type_vars:?}");
                        }
                    }
                    let sc = slf.close_over(ret_type);
                    let (new, ()) = slf.insert_var(nm.clone(), nm_pos.clone(), sc, false)?;
                    slf = new;
                    println!("{:?}", slf.ctxt.types);
                }
                println!("final {:?}", slf.ctxt.types);
                Ok((slf, Scheme::concrete(Type::unit())))
            }
        }
    }

    fn define_kind(mut self, data: &'a Data<'a>, pat: Kind, type_vars: &mut HashMap<String, Type>) -> Infer<'a, Type> {
        let (fst_pos, fst, op_rst) = data.split_first();
        let (mut slf, mut kind, var_fst) = if let Some(k) = self.lookup_kind(fst) {
            (self, k, type_vars.contains_key(fst))
        } else if fst.as_bytes() == b"->" {
            (self,
                if let Some(rst) = &op_rst {
                    rst.iter().fold(Kind::Type, |k, _| Kind::kfun(Kind::Type, k))
                } else {
                    Kind::kfun(Kind::Type, Kind::Type)
                }
                , false)
        } else if op_rst.is_some() {
            return self.err(fst_pos.clone(), UndefinedSymbol(fst));
        } else {
            let nm = Ident { pos: fst_pos.clone(), body: fst.clone() };
            if pat == Kind::Type {
                type_vars.entry(fst.clone())
                    .or_insert_with(|| self.fresh());
                (self, pat, true)
            } else {
                let (slf, ()) = self.insert_kind(nm, pat.clone())?;
                (slf, pat, false)
            }
        };
        
        if let Some(rst) = op_rst {
            let mut ts = Vec::new();
            for data in rst {
                match kind {
                    Kind::Type =>
                        return slf.err(fst_pos.clone(), TooManyArgs),
                    Kind::KFun(pk, rk) => {
                        let (new, t) = slf.define_kind(data, pk.as_ref().clone(), type_vars)?;
                        slf = new;
                        kind = *rk;
                        ts.push(t);
                    }
                }
            }
            Ok((slf, Type::Data(fst.clone(), ts)))
        } else {
            if var_fst {
                Ok((slf, type_vars.get(fst).cloned().unwrap()))
            } else {
                Ok((slf, Type::Data(fst.clone(), Vec::with_capacity(0))))
            }
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
                slf.err(x.pos, x.body),
        }
    }

    fn constraints(self, Expr { pos, body }: &'a Expr<'a>) -> Infer<'a, TContraints<'a>> {
        match body {
            ExprBody::Val(v) => 
                self.value_constraints(pos, v),
            ExprBody::SExp(es) => {
                let mut es = es.into_iter();
                if let Some(fst) = es.next() {
                    self.sexp_constraints(fst, es)
                } else {
                    Ok((self, null(Type::unit())))
                }
            }
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
    
    fn sexp_constraints(self, fst: &'a Expr<'a>, es: Iter<'a, Expr<'a>>) -> Infer<'a, TContraints<'a>> {
        let (mut slf, (mut last_t, mut cs)) = 
            self.constraints(fst)?;

        for e in es {
            let cnstr_pos = e.pos.clone();

            let (new, (arg_t, new_cs)) = 
                slf.constraints(e)?;
            
            slf = new;
            cs.extend(new_cs);
            
            let ret_type = slf.fresh();
            let body = (Type::fun(arg_t, ret_type.clone()), last_t);
            cs.push(Constraint { pos: cnstr_pos, body });
            
            last_t = ret_type;
        }

        Ok((slf, (last_t, cs)))
    }
}

fn flatten_args<'a>(Data { pos, body }: Data<'a>) -> TypeResult<'a, Vec<Ident<'a>>> {
    match body {
        DataBody::PSym(body) => 
            Ok(vec![Ident { pos, body }]),
        DataBody::PSExp(fst, rst) => {
            let mut args = vec![fst];
            for Data { pos, body } in rst {
                match body {
                    DataBody::PSym(body) => 
                        args.push(Ident { body, pos }),
                    DataBody::PSExp(_, _) => 
                        return Err(TypeError::new(pos, NotYetImplemented(format!("Data Pattern Matching")))),
                }
            }
            Ok(args)
        }
    }
}