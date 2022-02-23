pub mod lex;
pub(crate) mod lex_error;


use std::fmt::Write;

use crate::exprs::types::Type;

use self::lex::Token;


#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePos<'a> {
    pub name: Option<&'a String>,
    pub row: usize,
    pub col: usize,
}

impl<'a> FilePos<'a> {
    pub fn new(name: Option<&'a String>) -> Self {
        Self { name, row: 1, col: 1 }
    }

    pub fn advance(&mut self, och: &Option<char>) {
        match och {
            None => {},
            Some('\n') => {
                self.row += 1;
                self.col = 1;
            },
            _ => self.col += 1,
        }
    }
}

impl<'a> std::fmt::Display for FilePos<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "./{}:{}:{}", 
            self.name.map(|n| n.as_str()).unwrap_or("anon"), 
            self.row,
            self.col)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ParseError<'a> {
    EndOnQuote,
    InSExp(FilePos<'a>, Box<ParseError<'a>>),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Value<'a> {
    Int(usize),
    Sym(String),
    Quot(Box<Expr<'a>>),
    Type(Type)
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Sym(s) => write!(f, "{}", s),
            Value::Quot(q) => write!(f, "'{}", q),
            Value::Type(t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expr<'a> {
    Val(Value<'a>),
    SExp(FilePos<'a>, Vec<Expr<'a>>),
}

impl<'a> std::fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Val(v) => write!(f, "{}", v),
            Expr::SExp(_, es) => {
                f.write_char('(')?;
                if !es.is_empty() {
                    write!(f, "{}", es[0])?;
                    for e in &es[1..] {
                        write!(f, " {}", e)?;
                    }
                }
                f.write_char(')')
            },
        }
    }
}

pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> Result<Vec<Expr<'a>>, ParseError<'a>> {
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
                let sexp = parse_tokens(ts)
                    .map_err(|e| ParseError::InSExp(fp.clone(), Box::new(e)))?;
                Some(Expr::SExp(fp, sexp))
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