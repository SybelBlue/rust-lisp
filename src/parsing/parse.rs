use crate::types::Type;

use super::{lex::Token, FilePos};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ParseError<'a> {
    EndOnQuote,
    InSExp(FilePos<'a>, Box<ParseError<'a>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Value {
    Int(usize),
    Sym(String),
    Quot(Box<Expr>),
    Type(Type)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expr {
    Val(Value),
    SExp(Vec<Expr>),
}

pub fn parse_token<'a>(ts: Vec<Token<'a>>) -> Result<Vec<Expr>, ParseError<'a>> {
    let mut ts = ts.into_iter();
    let mut vals = Vec::new();
    let mut quot_count = 0;

    while let Some(t) = ts.next() {
        let val = match t {
            Token::Quote => {
                quot_count += 1;
                None
            },
            Token::Word(w) =>
                Some(if let Ok(n) = w.parse::<usize>() {
                    Expr::Val(Value::Int(n))
                } else {
                    Expr::Val(Value::Sym(w))
                }),
            Token::SExp(fp, ts) => {
                let sexp = parse_token(ts).map_err(|e| ParseError::InSExp(fp, Box::new(e)))?;
                Some(Expr::SExp(sexp))
            },
        };
        if let Some(val) = val {
            vals.push((0..quot_count).fold(val, |acc, _| Expr::Val(Value::Quot(Box::new(acc)))));
            quot_count = 0;
        }
    }

    if quot_count > 0 {
        Err(ParseError::EndOnQuote)
    } else {
        Ok(vals)
    }

}

// pub struct Context<'a> {
//     prev: Option<&'a Context<'a>>,
//     symbols: HashMap<String, crate::types::Type>,
// }

// impl<'a> Context<'a> {
//     pub fn new() -> Self {
//         use crate::types::Type::*;
//         Self {
//             prev: None,
//             symbols: vec!
//                 [ (format!("def"), Fun(Box::new(Str), Box::new(Type)))
//                 , (format!("+"), Fun(Box::new(Int), Box::new(Fun(Box::new(Int), Box::new(Int)))))
//                 ].into_iter().collect(),
//         }
//     }
// }