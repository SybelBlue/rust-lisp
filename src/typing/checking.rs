use crate::{errors::{TypeResult, TypeErrorBody::*, TypeError}, parsing::sources::FilePos, exprs::{Expr, Ident, SToken}, stmts::Stmt, values::Value};

use super::{Type, contexts::{Solver, Context}};

pub fn type_mod<'a>(stmts: &'a Vec<Stmt<'a>>, ctxt: Context) -> TypeResult<'a, (Vec<Type>, Context)> {
    let mut slvr = Solver::new(ctxt);
    let mut delayed = Vec::new();
    
    for s in stmts {
        delayed.push(match s {
            Stmt::Expr(e) => (e, None),
            Stmt::Bind(Ident { body: name, pos }, e) => {
                let (new, tvar) = slvr.bind_to_tvar(name.clone());
                slvr = new;
                (e, Some((pos, Type::Var(tvar))))
            }
        });
    }
    
    let mut types = Vec::new();

    for (e, op) in delayed {
        let (t, new) = type_expr(e, slvr)?;
        slvr = new;
        if let Some((pos, old_t)) = op {
            match slvr.unify(&old_t, &t) {
                Ok(new) => 
                    slvr = new,
                Err(e) => 
                    return Err(TypeError::new(pos.clone(), e)),
            }
        }
        types.push(t);
    }

    Ok((types.into_iter().map(|t| t.concretize(&slvr)).collect(), slvr.finish()))
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
