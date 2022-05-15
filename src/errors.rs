use std::{collections::HashSet, fmt::{Debug, Display, Formatter}};

use crate::{parsing::{sources::{FilePos, Loc}, lex::Keyword}, exprs::Expr, typing::Type};


pub type LexError<'a> = Loc<'a, LexErrorBody<'a>>;

#[derive(Debug, Clone)]
pub enum LexErrorBody<'a> {
    TooManyClosing,
    Unclosed(FilePos<'a>),
}

impl<'a> Display for LexErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooManyClosing => f.write_str("Extra Closing Parenthesis"),
            Self::Unclosed(fp) => {
                f.write_str("Unclosed Parens\nstarting\n")?;
                fp.write_snippet(f)?;
                f.write_str("ending")
            },
        }
    }
}

pub type ParseError<'a> = Loc<'a, ParseErrorBody<'a>>;

#[derive(Debug, Clone)]
pub enum ParseErrorBody<'a> {
    MisplacedKeyword(Keyword),
    MisplacedLiteral,
    MisplacedSExp,
    MissingIdentifier,
    BadBinding(String),
    DuplicateLambdaArg(String),
    InSExp(Box<ParseError<'a>>),
    NotYetImplemented(&'a str),
}

impl<'a> Display for ParseErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MisplacedKeyword(kw) => write!(f, "MisplacedKeyword({})", kw),
            Self::MisplacedLiteral => write!(f, "MisplacedLiteral"),
            Self::MisplacedSExp => write!(f, "MisplacedSExp"),
            Self::MissingIdentifier => write!(f, "MissingBindingIdentifier"),
            Self::BadBinding(w) => write!(f, "BadBinding: {}", w),
            Self::DuplicateLambdaArg(s) => write!(f, "DuplicateLambdaArg {}", s),
            Self::InSExp(sfp) => write!(f, "{}Inside SExpression", sfp.as_ref()),
            Self::NotYetImplemented(msg) => write!(f, "NotYetImplemented: {}", msg),
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
    NotYetImplemented(String),
    DuplicateNameAt(String, Option<FilePos<'a>>),
}

impl<'a> Display for TypeErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotYetImplemented(msg) =>
                f.write_str(msg.as_str()),
            Self::TooManyArgs(e) =>
                write!(f, "Too Many Arguments: {}", e),
            Self::TypeMismatch { got, expected } => 
                write!(f, "Type Mismatch\n\tgot:      {}\n\texpected: {}", got, expected),
            Self::UndefinedSymbol(s) => write!(f, "Undefined Symbol: {}", s),
            Self::InfiniteType(s, t) => {
                write!(f, "Infinite Type: ")?;
                let mut vals = HashSet::new();
                s.variable_values(&mut vals);
                t.variable_values(&mut vals);
                let map = Type::var_to_char_map(vals.into_iter().collect());
                s.display_with(f, &map, false)?;
                write!(f, " ~ ")?;
                t.display_with(f, &map, false)
            }
            Self::DuplicateNameAt(nm, op_loc) => {
                write!(f, "Duplicate Name: {nm}")?;
                if let Some(loc) = op_loc {
                    f.write_str("\noriginal definition ")?;
                    loc.write_snippet(f)
                } else {
                    Ok(())
                }
            }
        }
    }
}

pub type LexResult<'a, T> = Result<T, LexError<'a>>;
pub type TypeResult<'a, T> = Result<T, TypeError<'a>>;
pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;