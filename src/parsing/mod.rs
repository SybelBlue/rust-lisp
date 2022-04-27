pub mod lex;
pub mod sources;


use crate::{errors::ParseResult, exprs::{Stmt, Expr}};

use self::lex::Token;


pub fn parse<'a>(ts: Vec<Token<'a>>) -> ParseResult<'a, Vec<Stmt<'a>>> {
    todo!()    
}

fn parse_stmt<'a>(ts: Token<'a>) -> ParseResult<'a, Stmt<'a>> {
    todo!()    
}

fn parse_expr<'a>(ts: Token<'a>) -> ParseResult<'a, Expr<'a>> {
    todo!()    
}
