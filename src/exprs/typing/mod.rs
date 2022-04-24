pub mod checking;
pub mod contexts;

use std::{collections::{HashSet, HashMap}, fmt::{Write, Display, Formatter}};


use self::contexts::TypeContext;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Unit,
    Nat,
    Char,
    Type,
    Data(String),
    Var(usize),
    Fun(Box<Type>, Box<Type>),
}

impl Type {
    pub(crate) fn fun(p: Self, r: Self) -> Self {
        Self::Fun(Box::new(p), Box::new(r))
    }

    pub fn contains(&self, o: &Self) -> bool {
        if self == o {
            true
        } else if let Type::Fun(p, r) = self {
            p.contains(o) || r.contains(o)
        } else {
            false
        }
    }

    pub fn is_concrete(&self) -> bool {
        match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type => true,
            Type::Data(_) => true, // will maybe be polymorphic later
            Type::Fun(p, r) => p.is_concrete() && r.is_concrete(),
            Type::Var(_) => false,
        }
    }

    pub fn concretize(&self, ctxt: &TypeContext) -> Self {
        let out = match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type | Self::Data(_) => self.clone(),
            Type::Fun(p, r) => 
                Type::fun(p.concretize(ctxt), r.concretize(ctxt)),
            Type::Var(id) => ctxt.query_tvar(*id),
        };
        if out == *self {
            out
        } else {
            out.concretize(ctxt)
        }
    }

    pub fn alpha_sub(&self, tvar: usize, sub: &Self) -> Self {
        match self {
            Type::Var(x) if *x == tvar => sub.clone(),
            Type::Fun(p, r) => Type::fun(
                p.alpha_sub(tvar, sub), 
                r.alpha_sub(tvar, sub)
            ),
            _ => self.clone(),
        }
    }

    pub fn variable_values(&self, out: &mut HashSet<usize>) {
        match self {
            Self::Var(n) => { out.insert(*n); },
            Self::Fun(p, r) => {
                p.variable_values(out);
                r.variable_values(out);
            },
            Self::Data(_) => todo!(),
            _ => {}
        }
    }

    pub(crate) fn var_to_char_map(mut vals: Vec<usize>) -> HashMap<usize, char> {
        vals.sort();
        let map: HashMap<usize, char> = vals.into_iter()
            .enumerate()
            .map(|(i, l)| (l, (i as u8 + 'a' as u8) as char))
            .collect();
        if map.len() > 26 { todo!("sooooo many vars") }
        map
    }

    pub(crate) fn display_with(&self, f: &mut Formatter, map: &HashMap<usize, char>, wrap: bool) -> std::fmt::Result {
        match self {
            Type::Unit | Type::Nat | Type::Char | Type::Type | Type::Data(_) => self.fmt(f),
            Type::Var(n) => f.write_char(*map.get(n).unwrap()),
            Type::Fun(p, r) => {
                if wrap { f.write_char('(')?; }
                p.display_with(f, map, true)?;
                f.write_str(" -> ")?;
                r.display_with(f, map, false)?;
                if wrap { f.write_char(')')?; }
                Ok(())
            },
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Nat => write!(f, "Nat"),
            Type::Char => write!(f, "Char"),
            Type::Type => write!(f, "Type"),
            Type::Data(s) => write!(f, "{}", s),
            Type::Fun(_, _) => {
                let mut vals = HashSet::new();
                self.variable_values(&mut vals);
                self.display_with(f, &Type::var_to_char_map(vals.into_iter().collect()), true)
            },
            Type::Var(_) => f.write_char('a'),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unit => write!(f, "Unit"),
            Self::Nat => write!(f, "Nat"),
            Self::Char => write!(f, "Char"),
            Self::Type => write!(f, "Type"),
            Self::Data(arg0) => f.debug_tuple("Data").field(arg0).finish(),
            Self::Var(arg0) => write!(f, "t_{}", arg0),
            Self::Fun(arg0, arg1) => write!(f, "({:?} -> {:?})", arg0.as_ref(), arg1.as_ref()),
        }
    }
}
