use crate::{errors::{TypeResult, TypeErrorBody::*, TypeError}, parsing::sources::FilePos, exprs::{Expr, Ident, SToken}, stmts::Stmt, values::Value};

use super::{Type, contexts::{Solver, Context}};

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
                    let (new_t, new_slvr) = type_expr(e, slvr)?;
                    slvr = new_slvr;
                    let new_t = new_t.concretize(&slvr);
                    if new_t.improves(&t) {
                        match slvr.unify(&t, &new_t) {
                            Ok(new_slvr) => { 
                                slvr = new_slvr;
                                found = true;
                                if new_t.is_concrete() {
                                    types.push((i, new_t));
                                    continue;
                                }
                            },
                            Err(e) => 
                                return Err(TypeError::new(ident.pos.clone(), e)),
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
            let (slvr, tvar) = slvr.bind_to_tvar(name.clone());
            let s = Type::Var(tvar);
            let (t, slvr) = type_expr(e, slvr)?;
            if s == t {
                return Err(TypeError::new(pos.clone(), InfiniteType(s, t)));
            }
            match slvr.unify(&s, &t) {
                Ok(slvr) => Ok((t, slvr)),
                Err(e) => Err(TypeError::new(pos.clone(), e)),
            }
        }
    }
}

fn type_expr<'a>(e: &'a Expr, slvr: Solver) -> TypeResult<'a, (Type, Solver)> {
    match e {
        Expr::Val(v) => type_value(&v.body, slvr, &v.pos),
        Expr::SExp(SToken { pos, body }) => {
            if let Some((fst, rst)) = body.split_first() {
                let (mut target_type, mut slvr) = type_expr(fst, slvr)?;
                let mut rest = rst.into_iter();
                while let Some(curr_argument) = rest.next() {
                    let (arg_type, new) = type_expr(curr_argument, slvr)?;
                    slvr = new;
                    match target_type {
                        Type::Fun(param_type, ret_type) => {
                            match slvr.unify(param_type.as_ref(), &arg_type) {
                                Ok(new) => {
                                    target_type = *ret_type;
                                    slvr = new;
                                }
                                Err(e) => 
                                    return Err(TypeError::new(pos.clone(), e)),
                            }
                        }
                        Type::Var(_) => {
                            let (new, ret_type_id) = slvr.new_tvar();
                            let curr_expr_type = Type::fun(arg_type, Type::Var(ret_type_id));
                            match new.unify(&target_type, &curr_expr_type) {
                                Ok(new) => {
                                    target_type = Type::Var(ret_type_id);
                                    slvr = new;
                                }
                                Err(e) => 
                                    return Err(TypeError::new(pos.clone(), e)),
                            }
                        }
                        _ => return Err(TypeError::new(pos.clone(), TooManyArgs(fst))),
                    }
                }
                Ok((target_type.concretize(&slvr), slvr))
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
            let mut slvr = slvr.clone();
            let mut expr_type = Vec::new(); // in reverse order!
            for p in ps.as_ref().get_lambda_param_names() {
                let (new, var) = slvr.bind_to_tvar(p.clone());
                slvr = new;
                expr_type.push(Type::Var(var));
            }
            let (slvr, ret_type_var) = slvr.new_tvar();
            let ret_type_var = Type::Var(ret_type_var);
            let (ret_type, slvr) = type_expr(b, slvr)?;
            match slvr.unify(&ret_type, &ret_type_var) {
                Ok(slvr) => {
                    // undoes reversal!
                    let lam_type = expr_type
                        .into_iter()
                        .fold(ret_type, |arr, curr| Type::fun(curr, arr));
                    (lam_type.concretize(&slvr), slvr)
                }
                Err(e) => 
                    return Err(TypeError::new(pos.clone(), e)),
            }
        }
    })
}
