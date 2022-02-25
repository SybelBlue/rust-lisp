pub mod lex;
pub(crate) mod lex_error;

use crate::exprs::{Expr, values::Value, SBody};

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
    MisplacedQuote(FilePos<'a>),
    InSExp(FilePos<'a>, Box<ParseError<'a>>),
}

pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> Result<Vec<Expr<'a>>, ParseError<'a>> {
    let mut ts = ts.into_iter();
    let mut vals = Vec::new();
    let mut quoted = None;

    while let Some(t) = ts.next() {
        let val = match t {
            Token::Quote(fp) => {
                if quoted.is_some() {
                    return Err(ParseError::MisplacedQuote(fp));
                } else {
                    quoted = Some(fp);
                }
                None
            },
            Token::Word(w) =>
                Some(if let Ok(n) = w.parse::<usize>() {
                    Expr::Val(Value::Int(n))
                } else {
                    Expr::Val(Value::Sym(w))
                }),
            Token::SExp(SBody { start, body }) => {
                let body = parse_tokens(body)
                    .map_err(|e| ParseError::InSExp(start.clone(), Box::new(e)))?;
                Some(Expr::SExp(SBody { start, body }))
            },
        };
        if let Some(val) = val {
            vals.push(Expr::Val(Value::Quot(Box::new(val))));
            quoted = None;
        }
    }

    if let Some(fp) = quoted {
        Err(ParseError::MisplacedQuote(fp))
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