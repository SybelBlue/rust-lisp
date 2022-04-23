use std::{path::Path, fs::File, io::Read, ffi::OsStr};

use super::lex::SourceIter;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FilePos<'a> {
    pub name: Option<&'a String>,
    pub row: usize,
    pub col: usize,
}

impl<'a> FilePos<'a> {
    pub fn new(name: Option<&'a String>) -> Self {
        Self { name, row: 1, col: 1 }
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
            self.name.unwrap_or(&String::from("anon")), 
            self.row,
            self.col)
    }
}

#[derive(Debug, Clone)]
pub enum Source<'a> {
    Anon(&'a str),
    File(String)
}

impl<'a> Source<'a> {
    pub fn into_iter(&'a self) -> std::io::Result<SourceIter<'a>> {
        match self {
            Source::Anon(s) => Ok(SourceIter::new(s, None)),
            Source::File(p) => {
                let mut f = File::open(p)?;
                let mut s = String::new();
                f.read_to_string(&mut s);
                todo!("howwww")
            },
        }
    }
}

