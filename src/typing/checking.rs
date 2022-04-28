use std::collections::VecDeque;

use crate::{errors::{TypeResult, TypeErrorBody::*, TypeError}, parsing::sources::FilePos, exprs::{Stmt, Ident}};
use crate::exprs::{values::Value, Expr, SToken};

use super::{Type, contexts::{TypeContext, UnifyErr}};

pub fn type_mod<'a>(stmts: &'a Vec<Stmt<'a>>, ctxt: TypeContext) -> TypeResult<'a, (Vec<Type>, TypeContext)> {
    let mut ctxt = ctxt;
    let mut delayed = VecDeque::with_capacity(stmts.len());
    let mut types = Vec::new();
    for (i, s) in stmts.iter().enumerate() {
        if s.free_symbols().into_iter().all(|n| ctxt.has(n)) {
            let (t, new) = type_stmt(s, ctxt)?;
            ctxt = new;
            types.push((i, t));
        } else {
            let (new, tvar) = if let Stmt::Bind(ident, _) = s {
                ctxt.bind_to_tvar(ident.body.clone())
            } else {
                ctxt.new_tvar()
            };
            ctxt = new;
            delayed.push_back((i, s, Type::Var(tvar)));
        }
    }

    while let Some((i, stmt, t)) = delayed.pop_front() {
        match stmt {
            Stmt::Expr(e) => {
                ctxt = type_expr(e, ctxt)?.1;
            }
            Stmt::Bind(ident, e) => {
                let (new_t, new_ctxt) = type_expr(e, ctxt)?;
                ctxt = new_ctxt;
                if new_t.improves(&t) {
                    match ctxt.unify(&t, &new_t) {
                        Ok(new_ctxt) => { 
                            ctxt = new_ctxt;
                            if !new_t.is_concrete() {
                                delayed.push_back((i, stmt, new_t));
                            } else{
                                types.push((i, new_t));
                            }
                        },
                        Err(e) => return Err(TypeError::new(ident.pos.clone(), match e {
                            UnifyErr::Inf => 
                                InfiniteType(t, new_t),
                            UnifyErr::Mis => 
                                TypeMismatch { got: new_t, expected: t },
                        })),
                    }
                }
            }
        }
    }
    types.sort_by_key(|(i, _)| *i);
    return Ok((types.into_iter().map(|(_, t)| t).collect(), ctxt));
}

pub(crate) fn type_stmt<'a>(s: &'a Stmt<'a>, ctxt: TypeContext) -> TypeResult<'a, (Type, TypeContext)> {
    match s {
        Stmt::Expr(e) => 
            type_expr(e, ctxt),
        Stmt::Bind(Ident { body: name, pos }, e) => {
            let (ctxt, tvar) = ctxt.bind_to_tvar(name.clone());
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
                        todo!("shouldn't be possible, unifying var"),
                },
            }
        }
    }
}

fn type_expr<'a>(e: &'a Expr, ctxt: TypeContext) -> TypeResult<'a, (Type, TypeContext)> {
    match e {
        Expr::Val(v) => type_value(&v.body, ctxt, &v.pos),
        Expr::SExp(SToken { pos, body }) => {
            if let Some((fst, rst)) = body.0.split_first() {
                let (mut target_type, mut ctxt) = type_expr(fst, ctxt)?;
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
                Ok((Type::Unit, ctxt))
            }
        }
    }
}

fn type_value<'a>(v: &'a Value, ctxt: TypeContext, pos: &'a FilePos<'a>) -> TypeResult<'a, (Type, TypeContext)> {
    Ok(match v {
        Value::Nat(_) => (Type::Nat, ctxt),
        Value::Sym(k) => (
            ctxt.get(k).ok_or(TypeError::new(pos.clone(), UndefinedSymbol(k)))?.concretize(&ctxt),
            ctxt,
        ),
        Value::Lam(ps, b) => {
            let mut ctxt = ctxt.clone();
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
                Err(UnifyErr::Inf) => return Err(TypeError::new(pos.clone(), InfiniteType(ret_type, ret_type_var))),
                Err(UnifyErr::Mis) => todo!("Mismatch should be impossible with var & __"),
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
