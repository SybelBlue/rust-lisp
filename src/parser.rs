use crate::token::Token;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {

}


use ParseError::*;

impl std::fmt::Display for ParseError {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // match self {
        //     Eof(x) => write!(f, "Expected {} before end of file", x),
        // }
        unimplemented!()
    }
}

pub fn parse(_tkn: Token) {
    unimplemented!()
}