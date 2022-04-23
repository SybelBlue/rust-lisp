use std::{collections::HashSet, fmt::{Debug, Display, Formatter}};

use crate::{parsing::{FilePos, lex::Source}, exprs::{Expr, types::Type}};


pub trait Locable<'a> {
    fn loc(&'a self) -> &'a FilePos<'a>;
}

impl<'a> Locable<'a> for FilePos<'a> {
    fn loc(&'a self) -> &'a FilePos<'a> {
        self
    }
}

impl<'a> Locable<'a> for Source<'a> {
    fn loc(&'a self) -> &'a FilePos<'a> {
        &self.pos
    }
}

#[derive(Debug, Clone)]
pub struct Loc<L, T> {
    locable: L,
    body: T,
}

impl<'a, L: Locable<'a>, T: Debug + Clone + Display> Loc<L, T> {
    pub fn new(locable: L, body: T) -> Self {
        Self { locable, body }
    }
}

impl<'a, L: Locable<'a>, T: Debug + Clone + Display> Display for Loc<L, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} at {}", self.body, self.locable.loc())
    }
}

pub type LexError<'a> = Loc<Source<'a>, LexErrorBody>;

#[derive(Debug, Clone)]
pub enum LexErrorBody {
    TooManyClosing,
    Unclosed,
    StartingLambda,
}

impl Display for LexErrorBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(
            match self {
                Self::TooManyClosing => "Extra Closing Parenthesis",
                Self::Unclosed => "Unclosed S-Expression",
                Self::StartingLambda => "Starting Lambda Slash",
            })
    }
}

pub type ParseError<'a> = Loc<FilePos<'a>, ParseErrorBody<'a>>;

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