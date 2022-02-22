use std::collections::HashMap;

use super::lex::Token;

pub type ParseResult<T> = Result<T, ParseError>;

pub type ParseError = ();

pub struct Context<'a> {
    prev: Option<&'a Context<'a>>,
    symbols: HashMap<String, crate::types::Type<'a>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        use crate::types::Type::*;
        Self {
            prev: None,
            symbols: vec!
                [ (format!("def"), Fun(Box::new(Str), Box::new(Type)))
                , (format!("+"), Fun(Box::new(Int), Box::new(Fun(Box::new(Int), Box::new(Int)))))
                ].into_iter().collect(),
        }
    }
}

pub fn parse_all<'a>(ts: Vec<Token<'a>>, ctxt: &mut Context<'a>) -> ParseResult<()> {
    ts.into_iter().try_for_each(|t| parse(t, ctxt))
}

pub fn parse<'a>(t: Token<'a>, ctxt: &mut Context<'a>) -> ParseResult<()> {
    todo!()
}