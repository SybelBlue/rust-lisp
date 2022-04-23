use std::{fs::File, io::Read, fmt::{Display, Formatter}};

use crate::errors::LexResult;

use super::lex::{SourceIter, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilePos<'a> {
    pub src: &'a Source<'a>,
    pub row: usize,
    pub col: usize,
}

impl<'a> FilePos<'a> {
    pub fn new(src: &'a Source<'a>) -> Self {
        Self { src, row: 1, col: 1 }
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

    pub fn col_arrow(&self) -> String {
        let mut out = String::new();
        out.extend((0..(self.col - 2)).map(|_| ' '));
        out.push('^');
        out
    }

    pub fn write_snippet(&'a self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'a> Display for FilePos<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", 
            match self.src {
                Source::Anon(_) => "anon",
                Source::File(s) => s.as_str(),
            }, 
            self.row,
            self.col)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Source<'a> {
    Anon(&'a str),
    File(String),
}

impl<'a> Source<'a> {
    pub fn lex(&'a self, file_buf: &'a mut String) -> LexResult<'a, Vec<Token<'a>>> {
        SourceIter {
            pos: FilePos::new(self),
            txt: match self {
                Source::Anon(s) => s.chars(),
                Source::File(p) => {
                    let mut file = File::open(p).unwrap();
                    file.read_to_string(file_buf).unwrap();
                    file_buf.chars()
                },
            },
        }
        .lex()
    }
}

