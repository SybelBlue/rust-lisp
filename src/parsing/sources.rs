use std::{fs::File, io::{Read, BufRead}, fmt::{Display, Formatter}, hash::{Hash, Hasher}};

use crate::{errors::LexResult, parsing::lex::{SourceIter, Token}};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loc<'a, T> {
    pub pos: FilePos<'a>,
    pub body: T,
}

pub fn bodies<'a, T>(locs: &'a Vec<Loc<'a, T>>) -> Vec<&'a T> {
    locs.iter().map(|l| &l.body).collect()
}

impl<'a, T: Display> Loc<'a, T> {
    pub(crate) fn new(pos: FilePos<'a>, body: T) -> Self {
        Self { pos, body }
    }
}

impl<'a, T: Display> Display for Loc<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} ", self.body)?;
        self.pos.write_snippet(f)
    }
}

impl<'a, T: Hash> Hash for Loc<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pos.hash(state);
        self.body.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FilePos<'a> {
    pub src: &'a Source<'a>,
    pub row: usize,
    pub col: usize,
}

impl<'a> FilePos<'a> {
    pub(crate) fn new(src: &'a Source<'a>) -> Self {
        Self { src, row: 1, col: 1 }
    }

    pub(crate) fn advance(&mut self, och: &Option<char>) {
        match och {
            None => {},
            Some('\n') => {
                self.row += 1;
                self.col = 1;
            },
            _ => self.col += 1,
        }
    }

    fn col_arrow(&self) -> String {
        let mut out = String::new();
        out.extend((0..self.col.saturating_sub(2)).map(|_| ' '));
        out.push('^');
        out
    }

    pub(crate) fn write_snippet(&'a self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let line = self.src.get_line(self.row).ok_or(std::fmt::Error)?;
        let line_indicator = format!(" {} ", self.row);
        let margin: String = (0..line_indicator.len()).map(|_| ' ').collect();
        writeln!(f, "at {}", self)?;
        writeln!(f, "{}| ", margin)?;
        writeln!(f, "{}| {}", line_indicator, line)?;
        writeln!(f, "{}| {}", margin, self.col_arrow())
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    pub(crate) fn get_line(&self, row: usize) -> Option<String> {
        match self {
            Source::Anon(src) => {
                src.lines().nth(row - 1).map(String::from)
            },
            Source::File(p) => {
                File::open(p)
                    .ok()
                    .map(std::io::BufReader::new)
                    .and_then(|file| {
                        file.lines()
                            .nth(row - 1)
                            .and_then(Result::ok)
                    })
            },
        }
    }
}

