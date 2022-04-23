use std::{fs::File, io::Read};

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
}

impl<'a> std::fmt::Display for FilePos<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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
    File(String)
}

impl<'a> Source<'a> {
    pub(crate) fn into_iter(&'a self) -> std::io::Result<SourceIter<'a>> {
        match self {
            Source::Anon(s) => Ok(SourceIter {
                pos: FilePos::new(self),
                txt: s.chars()
            }),
            Source::File(p) => {
                let mut f = File::open(p)?;
                let mut s = String::new();
                f.read_to_string(&mut s);
                todo!("howwww")
            },
        }
    }

    pub fn lex(&'a self) -> LexResult<'a, Vec<Token<'a>>> {
        self.into_iter().unwrap().lex()
    }
}

