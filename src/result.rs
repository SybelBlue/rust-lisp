use crate::{lexer::LexError, parser::ParseError, value::{Value, Ident}};

pub type EvalResult<T> = Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    InternalError(String),
    ValueError(Value, String),
    ArgError { f_name: String, recieved: usize, expected: usize },
    IllegalDefError(Ident),
    NameError(Ident),
    LexError(LexError),
    ParseError(ParseError)
}

impl Error {
    pub fn name_error(s: String, fp: FilePos) -> Self {
        Self::NameError(Ident::new(s, fp))
    }

    pub fn type_error(msg: &str, fp: FilePos) -> Self {
        Self::ParseError(ParseError::TypeError(Ident::new(String::from(msg), fp)))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InternalError(s) => write!(f, "InternalError: {}", s),
            Error::ValueError(v, s) => write!(f, "ValueError({}): {}", v, s),
            Error::ArgError { f_name, recieved, expected } => 
                write!(f, "ArgError({}): recieved {} arguments, expected {}", f_name, recieved, expected),
            Error::IllegalDefError(n) => 
                write!(f, "IllegalDefError({}): cannot define in immutable scope at {}", n.name, n.file_pos),
            Error::NameError(n) => write!(f, "NameError({}): not defined in scope at {}", n.name, n.file_pos),
            Error::LexError(p) => write!(f, "LexError: {}", p),
            Error::ParseError(p) => write!(f, "ParseError: {}", p),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePos {
    pub col: usize,
    pub line: usize,
}

impl FilePos {
    pub fn new() -> Self {
        Self { col: 1, line: 1 }
    }

    pub fn advance(&mut self, out: &char) {
        if out == &'\n' || out == &'\r' {
            self.col = 1;
            self.line += 1;
        } else {
            self.col += 1;
        }
    }
}

impl std::fmt::Display for FilePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}