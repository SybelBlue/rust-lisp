use crate::{result::FilePos, value::Ident};

#[derive(Debug, Clone)]
pub enum Token {
    Lit(FilePos, i64),
    Sym(Ident),
    Form(FilePos, Vec<Token>),
}

impl Token {
    pub fn new(ident: Ident) -> Self {
        if let Ok(x) = ident.name.parse() {
            Self::Lit(ident.file_pos, x)
        } else {
            Self::Sym(ident)
        }
    }

    pub fn is_sym(&self, s: &str) -> bool {
        if let Self::Sym(id) = self {
            id.name.as_str() == s
        } else {
            false
        }
    }

    pub fn file_pos(&self) -> &FilePos {
        match self {
            Self::Lit(fp, _) => fp,
            Self::Sym(ident) => &ident.file_pos,
            Self::Form(fp, _) => fp,
        }
    }
}
