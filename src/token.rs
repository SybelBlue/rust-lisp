use crate::{result::FilePos, value::Ident};

#[derive(Debug, Clone)]
pub enum Token {
    Lit(FilePos, i64),
    ColCol(FilePos),
    Arrow(FilePos),
    Sym(Ident),
    Form(Vec<Token>),
}

impl Token {
    pub fn new(ident: Ident) -> Self {
        match ident.name.as_bytes() {
            b"->" => Self::Arrow(ident.file_pos),
            b"::" => Self::ColCol(ident.file_pos),
            _ => 
                if let Ok(x) = ident.name.parse() {
                    Self::Lit(ident.file_pos, x)
                } else {
                    Self::Sym(ident)
                }
        }
    }
}