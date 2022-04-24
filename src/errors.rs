use std::{collections::HashSet, fmt::{Debug, Display, Formatter}};

use crate::{parsing::{sources::FilePos}, exprs::{Expr, typing::Type}};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loc<'a, T> {
    pub pos: FilePos<'a>,
    pub body: T,
}

impl<'a, T: Debug + Clone + Display> Loc<'a, T> {
    pub(crate) fn new(pos: FilePos<'a>, body: T) -> Self {
        Self { pos, body }
    }

    pub(crate) fn display_simple(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.body, f)
    }
}

impl<'a, T: Display> Display for Loc<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} ", self.body)?;
        self.pos.write_snippet(f)
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
            Self::Unclosed(fp) => {
                f.write_str("Unclosed Parens starting at:\n")?;
                fp.write_snippet(f)?;
                f.write_str("ending at:\n")
            },
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
            Self::MisplacedLambda => write!(f, "MisplacedLambda"),
            Self::MissingLambdaParams => write!(f, "MissingLambdaParams"),
            Self::MissingLambdaBody => write!(f, "MissingLambdaBody"),
            Self::ExtraLambdaBody => write!(f, "ExtraLambdaBody"),
            Self::DuplicateLambdaArg(s) => write!(f, "DuplicateLambdaArg {}", s),
            Self::InSExp(sfp) => {
                if let Self::InSExp(inner) = &sfp.body {
                    if sfp.pos.row == inner.pos.row {
                        return Display::fmt(inner.as_ref(), f);
                    }
                }
                write!(f, "In S-Expression:\n{}", sfp.body)
            },
        }
    }
}

pub type TypeError<'a> = Loc<'a, TypeErrorBody<'a>>;

#[derive(Debug, Clone)]
pub enum TypeErrorBody<'a> {
    TooManyArgs(&'a Expr<'a>),
    TypeMismatch { got: Type, expected: Type },
    InfiniteType(Type, Type),
    UndefinedSymbol(&'a String),
}

impl<'a> Display for TypeErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooManyArgs(e) => {
                f.write_str("Too Many Arguments: ")?;
                e.display_simple(f)
            }
            Self::TypeMismatch { got, expected } => 
                write!(f, "Type Mismatch\n\tgot:      {}\n\texpected: {}", got, expected),
            Self::UndefinedSymbol(s) => write!(f, "Undefined Symbol: {}", s),
            Self::InfiniteType(s, t) => {
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