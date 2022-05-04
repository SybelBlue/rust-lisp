use crate::{errors::{TypeResult, TypeErrorBody::*, TypeError}, parsing::sources::FilePos, exprs::{Expr, Ident, SToken}, stmts::Stmt, values::Value};

use super::{Type, contexts::{Solver, Context, UnifyErr}};

pub fn type_mod<'a>(stmts: &'a Vec<Stmt<'a>>, ctxt: Context) -> TypeResult<'a, (Vec<Type>, Context)> {
    let mut slvr = Solver::new(ctxt);
    let mut delayed = Vec::with_capacity(stmts.len());
    let mut types = Vec::new();
    for (i, s) in stmts.iter().enumerate() {
        if s.free_symbols().into_iter().all(|n| slvr.has(n)) {
            let (t, new) = type_stmt(s, slvr)?;
            slvr = new;
            types.push((i, t));
        } else {
            let (new, tvar) = if let Stmt::Bind(ident, _) = s {
                slvr.bind_to_tvar(ident.body.clone())
            } else {
                slvr.new_tvar()
            };
            slvr = new;
            delayed.push((i, s, Type::Var(tvar)));
        }
    }

    loop {
        let mut found = false;
        let mut next_delayed = Vec::new();
        for (i, stmt, t) in delayed {
            match stmt {
                Stmt::Expr(e) => {
                    let (new_t, new_slvr) = type_expr(e, slvr)?;
                    slvr = new_slvr;
                    types.push((i, new_t));
                }
                Stmt::Bind(ident, e) => {
                    let (new_t, new_ctxt) = type_expr(e, slvr)?;
                    slvr = new_ctxt;
                    let new_t = new_t.concretize(&slvr);
                    if new_t.improves(&t) {
                        match slvr.unify(&t, &new_t) {
                            Ok(new_ctxt) => { 
                                slvr = new_ctxt;
                                found = true;
                                if new_t.is_concrete() {
                                    types.push((i, new_t));
                                    continue;
                                }
                            },
                            Err(e) => {
                                return Err(
                                    TypeError::new(ident.pos.clone(), 
                                        match e {
                                            UnifyErr::Inf => 
                                                InfiniteType(t, new_t),
                                            UnifyErr::Mis => 
                                                TypeMismatch { got: new_t, expected: t },
                                        })
                                )
                            },
                        }
                    }
                    next_delayed.push((i, stmt, new_t));
                }
            }
        }

        if found && !next_delayed.is_empty() {
            delayed = next_delayed;
        } else {
            types.extend(next_delayed.into_iter().map(|(i, _, t)| (i, t)));
            types.sort_by_key(|(i, _)| *i);
            return Ok((types.into_iter().map(|(_, t)| t.concretize(&slvr)).collect(), slvr.finish()));
        }
    }
}

fn type_stmt<'a>(s: &'a Stmt<'a>, slvr: Solver) -> TypeResult<'a, (Type, Solver)> {
    match s {
        Stmt::Expr(e) => 
            type_expr(e, slvr),
        Stmt::Bind(Ident { body: name, pos }, e) => {
            let (ctxt, tvar) = slvr.bind_to_tvar(name.clone());
            let s = Type::Var(tvar);
            let (t, ctxt) = type_expr(e, ctxt)?;
            if s == t {
                return Err(TypeError::new(pos.clone(), InfiniteType(s, t)));
            }
            match ctxt.unify(&s, &t) {
                Ok(ctxt) => Ok((t, ctxt)),
                Err(e) => match e {
                    UnifyErr::Inf => 
                        Err(TypeError::new(pos.clone(), InfiniteType(s, t))),
                    UnifyErr::Mis => 
                        Err(TypeError::new(pos.clone(), TypeMismatch { expected: s, got: t })),
                },
            }
        }
    }
}

fn type_expr<'a>(e: &'a Expr, slvr: Solver) -> TypeResult<'a, (Type, Solver)> {
    match e {
        Expr::Val(v) => type_value(&v.body, slvr, &v.pos),
        Expr::SExp(SToken { pos, body }) => {
            if let Some((fst, rst)) = body.split_first() {
                let (mut target_type, mut ctxt) = type_expr(fst, slvr)?;
                let mut rest = rst.into_iter();
                while let Some(curr_argument) = rest.next() {
                    let (arg_type, new) = type_expr(curr_argument, ctxt)?;
                    ctxt = new;
                    match target_type {
                        Type::Fun(param_type, ret_type) => {
                            match ctxt.unify(param_type.as_ref(), &arg_type) {
                                Ok(new) => {
                                    target_type = *ret_type;
                                    ctxt = new;
                                }
                                Err(UnifyErr::Mis) => {
                                    return Err(TypeError::new(pos.clone(), TypeMismatch {
                                        got: arg_type,
                                        expected: *param_type,
                                    }));
                                }
                                Err(UnifyErr::Inf) => {
                                    return Err(TypeError::new(pos.clone(), InfiniteType(param_type.as_ref().clone(), arg_type)));
                                }
                            }
                        }
                        Type::Var(_) => {
                            let (new, ret_type_id) = ctxt.new_tvar();
                            let curr_expr_type = Type::fun(arg_type, Type::Var(ret_type_id));
                            match new.unify(&target_type, &curr_expr_type) {
                                Ok(new) => {
                                    target_type = Type::Var(ret_type_id);
                                    ctxt = new;
                                }
                                Err(UnifyErr::Mis) => {
                                    return Err(TypeError::new(pos.clone(), TypeMismatch {
                                        got: curr_expr_type,
                                        expected: target_type,
                                    }));
                                }
                                Err(UnifyErr::Inf) => {
                                    return Err(TypeError::new(pos.clone(), InfiniteType(target_type, curr_expr_type)));
                                }
                            }
                        }
                        _ => return Err(TypeError::new(pos.clone(), TooManyArgs(fst))),
                    }
                }
                Ok((target_type.concretize(&ctxt), ctxt))
            } else {
                Ok((Type::Unit, slvr))
            }
        }
    }
}

fn type_value<'a>(v: &'a Value, slvr: Solver, pos: &'a FilePos<'a>) -> TypeResult<'a, (Type, Solver)> {
    Ok(match v {
        Value::Nat(_) => (Type::Nat, slvr),
        Value::Char(_) => (Type::Char, slvr),
        Value::Sym(k) => {
            let (slvr, op_t) = slvr.get(k);
            let t = op_t
                .ok_or(TypeError::new(pos.clone(), UndefinedSymbol(k)))?
                .concretize(&slvr);
            (t, slvr)
        }
        Value::Lam(ps, b) => {
            let mut ctxt = slvr.clone();
            let mut expr_type = Vec::new(); // in reverse order!
            for p in ps.as_ref().get_lambda_param_names() {
                let (new, var) = ctxt.bind_to_tvar(p.clone());
                ctxt = new;
                expr_type.push(Type::Var(var));
            }
            let (ctxt, ret_type_var) = ctxt.new_tvar();
            let ret_type_var = Type::Var(ret_type_var);
            let (ret_type, ctxt) = type_expr(b, ctxt)?;
            match ctxt.unify(&ret_type, &ret_type_var) {
                Err(UnifyErr::Inf) => 
                    return Err(TypeError::new(pos.clone(), 
                        InfiniteType(ret_type, ret_type_var))),
                Err(UnifyErr::Mis) => 
                    return Err(TypeError::new(pos.clone(), 
                        TypeMismatch { got: ret_type_var, expected: ret_type })),
                Ok(ctxt) => {
                    // undoes reversal!
                    let lam_type = expr_type
                        .into_iter()
                        .fold(ret_type, |arr, curr| Type::fun(curr, arr));
                    (lam_type.concretize(&ctxt), ctxt)
                }
            }
        }
    })
}
