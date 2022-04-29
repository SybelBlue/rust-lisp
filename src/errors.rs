use std::{collections::HashSet, fmt::{Debug, Display, Formatter}, hash::Hash};

use crate::{parsing::{sources::FilePos, lex::Keyword}, exprs::Expr, typing::Type};


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Loc<'a, T> {
    pub pos: FilePos<'a>,
    pub body: T,
}

impl<'a, T: Display> Loc<'a, T> {
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

impl<'a, T: Hash> Hash for Loc<'a, T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.pos.hash(state);
        self.body.hash(state);
    }
}

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
    MissingBindingIdentifier,
    BadBinding(String),
    DuplicateLambdaArg(String),
    InSExp(Box<ParseError<'a>>),
    NotYetImplemented(&'a str),
}

impl<'a> Display for ParseErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MisplacedKeyword(kw) => write!(f, "MisplacedKeyword({})", kw),
            Self::MissingBindingIdentifier => write!(f, "MissingBindingIdentifier"),
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
}

impl<'a> Display for TypeErrorBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TooManyArgs(e) =>
                write!(f, "Too Many Arguments: {}", e),
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