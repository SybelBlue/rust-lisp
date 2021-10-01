use crate::{result::FilePos, value::Ident};

#[derive(Debug, Clone)]
pub enum Token {
    Lit(FilePos, i64),
    Str(Ident),
    Form(Vec<Token>),
}