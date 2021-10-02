use crate::{value::*, token::Token, context::Context, result::*};

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    TypeError(Ident)
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeError(ident) => write!(f, "TypeError: {}", ident),
        }
    }
}

pub fn parse<'a>(tkns: Vec<Token>, ctxt: &'a Context<'a>) -> EvalResult<Expr> {
    let mut tkns = tkns.into_iter().peekable();
    let mut expr_tkns = Vec::new();
    while let Some(t) = tkns.next() {
        if t.is_sym("::") {
            break;
        }
        expr_tkns.push(parse_tkn(t, ctxt)?);
    }
    
    if let Some(t) = tkns.next() {
        if t.is_sym("::") {
            return Err(Error::NameError(Ident::new(format!("::"), *t.file_pos())));
        }
        println!("type {:?}", parse_type(t, ctxt, true)?);
    }
    
    if let Some(t) = tkns.next() {
        Err(Error::type_error("Only one type may be provided", *t.file_pos()))
    } else {
        Ok(Expr::Form(expr_tkns))
    }
}

pub fn parse_type<'a>(tkn: Token, ctxt: &'a Context<'a>, allow_constraint: bool) -> EvalResult<Type> {
    match tkn {
        Token::Lit(fp, _) => Err(Error::name_error(format!("Type must begin with letter"), fp)),
        Token::Form(_, tks) => {
            let mut ts = tks.into_iter().peekable();
            return match ts.next() {
                None => Ok(Type::Unit),
                Some(t) if t.is_sym("=>") => {
                    if !allow_constraint {
                        return Err(Error::name_error(format!("Constraint not allowed here"), *t.file_pos()));
                    }
                    let mut ts = ts.rev();
                    let res_tkn = ts.next().ok_or(Error::ArgError { f_name: format!("->"), recieved: 0, expected: 2})?;
                    let res_type = parse_type(res_tkn, ctxt, false)?;
                    ts.try_fold(res_type, |acc, tkn| parse_constraint(tkn, acc, ctxt))
                },
                Some(t) if t.is_sym("->") => {
                    let mut ts = ts.rev();
                    let res_tkn = ts.next().ok_or(Error::ArgError { f_name: format!("->"), recieved: 0, expected: 2})?;
                    let res_type = parse_type(res_tkn, ctxt, false)?;
                    ts.try_fold(res_type, |acc, tkn| {
                        Ok(Type::Arrow(Box::new(parse_type(tkn, ctxt, false)?), Box::new(acc)))
                    })
                },
                Some(Token::Sym(ident)) => Ok(Type::from_ident(ident)),
                Some(t) => Err(Error::type_error("A Type must begin with a Symbol", *t.file_pos()))
            }
        }
        Token::Sym(ident) => Ok(Type::from_ident(ident)),
    }
}

pub fn parse_constraint<'a>(tkn: Token, child: Type, ctxt: &'a Context<'a>) -> EvalResult<Type> {
    if let Token::Form(fp, tks) = tkn {
        let mut ts = tks.into_iter();
        let head_tkn = ts.next().ok_or(Error::type_error("A Constraint cannot be Empty", fp))?;
        let head = (if let Token::Sym(ident) = head_tkn { 
            Ok(ident) 
        } else { 
            Err(Error::type_error("A Constraint must begin with a Symbol", fp)) 
        })?;
        let mut rest = Vec::new();
        for t in ts {
            rest.push(parse_type(t, ctxt, false)?);
        }
        Ok(Type::Constraint(head, rest, Box::new(child)))
    } else {
        Err(Error::type_error("A Constraint must be a Form", *tkn.file_pos()))
    }
}

pub fn parse_tkn<'a>(tkn: Token, ctxt: &'a Context<'a>) -> EvalResult<Expr> {
    match tkn {
        Token::Lit(_, x) => Ok(Expr::Val(Value::Int(x))),
        Token::Form(_, tkns) => parse(tkns, ctxt),
        Token::Sym(ident) => Ok(Expr::Var(ident.name)),
    }
}