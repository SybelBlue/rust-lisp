use crate::{parser::ParseError, evaluator::{Ident, value::Value}};

pub type EvalResult<T> = Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    ValueError(Value, String),
    ArgError { f_name: String, recieved: usize, expected: usize },
    IllegalDefError(Ident),
    NameError(Ident),
    RedefError(Ident, String),
    ParseError(ParseError),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ValueError(v, s) => write!(f, "ValueError({}): {}", v, s),
            Error::ArgError { f_name, recieved, expected } => 
                write!(f, "ArgError({}): recieved {} arguments, expected {}", f_name, recieved, expected),
            Error::IllegalDefError(n) => 
                write!(f, "IllegalDefError({}): cannot define in immutable scope at {}", n.name, n.file_pos),
            Error::NameError(n) => write!(f, "NameError({}): not defined in scope at {}", n.name, n.file_pos),
            Error::RedefError(orig, n) => 
                write!(f, "RedefError({}): cannot redefine {} at {}", n, orig.name, orig.file_pos),
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