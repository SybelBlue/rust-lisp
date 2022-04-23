use std::{collections::HashSet, fmt::{Debug, Display, Formatter}};

use crate::{parsing::{sources::FilePos}, exprs::{Expr, types::Type}};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loc<'a, T> {
    pub pos: FilePos<'a>,
    pub body: T,
}

impl<'a, T: Debug + Clone + Display> Loc<'a, T> {
    pub fn new(pos: FilePos<'a>, body: T) -> Self {
        Self { pos, body }
    }
}

impl<'a, T: Display> Display for Loc<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} at {}", self.body, self.pos)
    }
}

pub type LexError<'a> = Loc<'a, LexErrorBody<'a>>;

#[derive(Debug, Clone)]
pub enum LexErrorBody<'a> {
    TooManyClosing,
    Unclosed(FilePos<'a>),
    StartingLambda,
}

impl<'a> Display for LexErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooManyClosing => f.write_str("Extra Closing Parenthesis"),
            Self::Unclosed(fp) => write!(f, "Unclosed Parens\n\tstarting at {}\n\tending", fp),
            Self::StartingLambda => f.write_str("Starting Lambda Slash"),
        }
    }
}

pub type ParseError<'a> = Loc<'a, ParseErrorBody<'a>>;

#[derive(Debug, Clone)]
pub enum ParseErrorBody<'a> {
    MisplacedLambda,
    MissingLambdaParams,
    MissingLambdaBody,
    ExtraLambdaBody,
    DuplicateLambdaArg(String),
    InSExp(Box<ParseError<'a>>),
}

impl<'a> Display for ParseErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorBody::MisplacedLambda => write!(f, "MisplacedLambda"),
            ParseErrorBody::MissingLambdaParams => write!(f, "MissingLambdaParams"),
            ParseErrorBody::MissingLambdaBody => write!(f, "MissingLambdaBody"),
            ParseErrorBody::ExtraLambdaBody => write!(f, "ExtraLambdaBody"),
            ParseErrorBody::DuplicateLambdaArg(s) => write!(f, "DuplicateLambdaArg {}", s),
            ParseErrorBody::InSExp(sfp) => write!(f, "In S-Expression:\n{}", sfp.body),
        }
    }
}


// impl<'a> Display for LexError<'a> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         let line_indicator = format!(" {} ", self.src.pos.row);
//         let margin: String = (0..line_indicator.len()).map(|_| ' ').collect();
//         write!(f, "error: {} at {}\n{}|\n{}| {}\n{}| {}", 
//             self.body.name(), 
//             self.src.pos, 
//             margin,
//             line_indicator, 
//             self.src.current_line(),
//             margin,
//             self.body.col_arrow(&self.src.pos))
//     }
// }

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

impl<'a> Display for TypeError<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

pub type LexResult<'a, T> = Result<T, LexError<'a>>;
pub type TypeResult<'a, T> = Result<T, TypeError<'a>>;
pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;