use crate::{errors::{TypeResult, TypeErrorBody::*, TypeError}, parsing::sources::FilePos};
use crate::exprs::{values::Value, Expr, SToken};

use super::{Type, contexts::{TypeContext, UnifyErr}};

pub fn type_expr<'a>(e: &'a Expr, ctxt: TypeContext) -> TypeResult<'a, (Type, TypeContext)> {
    match e {
        Expr::Data { name, kind, variants } => {
            // need a parse_expr instead that captures type variables
            // and returns a Type instead of evaluating the type of "(-> Nat Nat) ~> Type"
            let (kind, ctxt) = type_expr(kind, ctxt)?;

            let mut ctxt = ctxt.bind(name.body.clone(), kind);
            for v in variants {
                let (variant_type, new) = type_expr(&v.tipe, ctxt)?;
                let dtype_name = variant_type.return_type().datatype_name();
                if dtype_name != name.body {
                    return Err(TypeError::new(v.name.pos.clone(), DatatypeVariantReturnsNondata(dtype_name)))
                }
                ctxt = new.bind(v.name.body.clone(), variant_type);
            }
            Ok((Type::Unit, ctxt))
        },
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

pub fn type_value<'a>(v: &'a Value, ctxt: TypeContext, pos: &'a FilePos<'a>) -> TypeResult<'a, (Type, TypeContext)> {
    Ok(match v {
        Value::Nat(_) => (Type::Nat, ctxt),
        Value::Type(_) => (Type::Type, ctxt),
        Value::Sym(k) => (
            ctxt.get(k).ok_or(TypeError::new(pos.clone(), UndefinedSymbol(k)))?.concretize(&ctxt),
            ctxt,
        ),
        Value::Lam(ps, b) => {
            let mut ctxt = ctxt.clone();
            let mut expr_type = Vec::new(); // in reverse order!
            for p in ps.as_ref().get_lambda_param_names() {
                let (new, var) = ctxt.bind_to_tvar(p);
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