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
    MisplacedLambda(FilePos<'a>),
    MissingLambdaParams(FilePos<'a>),
    MissingLambdaBody(FilePos<'a>),
    ExtraLamBody(FilePos<'a>),
    InSExp(FilePos<'a>, Box<ParseError<'a>>),
}

pub fn parse_tokens<'a>(ts: Vec<Token<'a>>) -> Result<Vec<Expr<'a>>, ParseError<'a>> {
    let mut ts = ts.into_iter();
    let mut exprs = Vec::new();

    if let Some(t) = ts.next() {
        match parse_first(t)? {
            Err(lam_fp) => {
                exprs.push(Expr::Val(Value::Sym(String::from("\\"))));
                let t = ts.next().ok_or(ParseError::MissingLambdaParams(lam_fp.clone()))?;
                exprs.push(parse_rest(t)?);
                let t = ts.next().ok_or(ParseError::MissingLambdaBody(lam_fp.clone()))?;
                exprs.push(parse_rest(t)?);
                return if ts.next().is_some() {
                    Err(ParseError::ExtraLamBody(lam_fp))
                } else {
                    Ok(exprs)
                };
            },
            Ok(e) => exprs.push(e)
        }
    } else {
        return Ok(exprs);
    }

    for t in ts {
        exprs.push(parse_rest(t)?);
    }

    Ok(exprs)
}

fn parse_rest<'a>(t: Token<'a>) -> Result<Expr<'a>, ParseError<'a>> {
    parse_first(t)?.map_err(ParseError::MisplacedLambda)
}

fn parse_first<'a>(t: Token<'a>) -> Result<Result<Expr<'a>, FilePos<'a>>, ParseError<'a>> {
    Ok(match t {
        Token::LamSlash(fp) => Err(fp),
        Token::Word(w) =>
            Ok(if let Ok(n) = w.parse::<usize>() {
                Expr::Val(Value::Int(n))
            } else {
                Expr::Val(Value::Sym(w))
            }),
        Token::SExp(SBody { start, body }) => {
            let body = parse_tokens(body)
                .map_err(|e| ParseError::InSExp(start.clone(), Box::new(e)))?;
            Ok(Expr::SExp(SBody { start, body }))
        },
    })
}