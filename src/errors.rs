use std::collections::HashSet;

use crate::{parsing::{FilePos, lex::Source}, exprs::{Expr, types::Type}};


#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum LexErrorBody<'a> {
    TooManyClosing,
    Unclosed(FilePos<'a>),
    StartingLambda(FilePos<'a>),
}

impl<'a> LexErrorBody<'a> {
    pub fn col_arrow(&self, file_pos: &FilePos) -> String {
        let file_pos = match &self {
            Self::TooManyClosing => file_pos,
            Self::Unclosed(fp) => fp,
            Self::StartingLambda(fp) => fp,
        };

        let mut out = String::new();
        out.extend((0..(file_pos.col - 2)).map(|_| ' '));
        out.push('^');
        out
    }

    pub fn name(&self) -> String {
        match self {
            Self::TooManyClosing => format!("Extra Closing Parenthesis"),
            Self::Unclosed(_) => format!("Unclosed S-Expression"),
            Self::StartingLambda(_) => format!("Starting Lambda Slash")
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum ParseError<'a> {
    MisplacedLambda(FilePos<'a>),
    MissingLambdaParams(FilePos<'a>),
    MissingLambdaBody(FilePos<'a>),
    ExtraLambdaBody(FilePos<'a>),
    DuplicateLambdaArg(String),
    InSExp(FilePos<'a>, Box<ParseError<'a>>),
}

impl<'a> std::fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::MisplacedLambda(fp) => write!(f, "MisplacedLambda at {}", fp),
            ParseError::MissingLambdaParams(fp) => write!(f, "MissingLambdaParams at {}", fp),
            ParseError::MissingLambdaBody(fp) => write!(f, "MissingLambdaBody at {}", fp),
            ParseError::ExtraLambdaBody(fp) => write!(f, "ExtraLambdaBody at {}", fp),
            ParseError::DuplicateLambdaArg(fp) => write!(f, "DuplicateLambdaArg at {}", fp),
            ParseError::InSExp(sfp, err) => write!(f, "In S-Expression at {}:\n{}", sfp, err.as_ref()),
        }
    }
}

#[derive(Debug)]
pub struct LexError<'a> {
    pub src: Source<'a>,
    pub(crate) body: LexErrorBody<'a>,
}

impl<'a> std::fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_indicator = format!(" {} ", self.src.pos.row);
        let margin: String = (0..line_indicator.len()).map(|_| ' ').collect();
        write!(f, "error: {} at {}\n{}|\n{}| {}\n{}| {}", 
            self.body.name(), 
            self.src.pos, 
            margin,
            line_indicator, 
            self.src.current_line(),
            margin,
            self.body.col_arrow(&self.src.pos))
    }
}

#[derive(Debug, Clone)]
pub enum TypeError<'a> {
    TooManyArgs(&'a FilePos<'a>, &'a Expr<'a>),
    TypeMismatch {
        got: Type,
        expected: Type,
        at: &'a FilePos<'a>,
    },
    InfiniteType(Type, Type),
    UndefinedSymbol(&'a String),
}

impl<'a> std::fmt::Display for TypeError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::TooManyArgs(pos, e) => write!(f, "TooManyArgs: {} at {}", e, pos),
            TypeError::TypeMismatch { got, expected, at } => write!(
                f, "TypeMismatch at {}\n\tgot:      {}\n\texpected: {}",
                at, got, expected),
            TypeError::UndefinedSymbol(s) => write!(f, "UndefinedSymbol: {}", s),
            TypeError::InfiniteType(s, t) => {
                write!(f, "InfiniteType: ")?;
                let mut vals = HashSet::new();
                s.variable_values(&mut vals);
                t.variable_values(&mut vals);
                let map = Type::var_to_char_map(vals.into_iter().collect());
                s.display_with(f, &map, false)?;
                write!(f, " ~ ")?;
                t.display_with(f, &map, false)
            }
        }
    }
}

pub type TypeResult<'a, T> = Result<T, TypeError<'a>>;