pub mod checking;
pub mod contexts;

use std::{collections::{HashSet, HashMap}, fmt::{Write, Display, Formatter}};


use self::contexts::TypeContext;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Unit,
    Nat,
    Char,
    Data(String, Box<Type>), // eg Data("Vec", [Nat -> Type -> Type])
    Var(usize),
    Fun(Box<Type>, Box<Type>),
}

impl Type {
    pub(crate) fn fun(p: Self, r: Self) -> Self {
        Self::Fun(Box::new(p), Box::new(r))
    }

    pub(crate) fn contains(&self, o: &Self) -> bool {
        if self == o {
            true
        } else if let Type::Fun(p, r) = self {
            p.contains(o) || r.contains(o)
        } else {
            false
        }
    }

    pub(crate) fn concretize(&self, ctxt: &TypeContext) -> Self {
        let out = match self {
            Self::Unit | Self::Nat | Self::Char => self.clone(),
            Self::Data(_, t) => t.as_ref().clone(),
            Self::Fun(p, r) => 
                Self::fun(p.concretize(ctxt), r.concretize(ctxt)),
            Self::Var(id) => ctxt.query_tvar(*id),
        };
        if out == *self {
            out
        } else {
            out.concretize(ctxt)
        }
    }

    pub(crate) fn variable_values(&self, out: &mut HashSet<usize>) {
        match self {
            Self::Var(n) => { out.insert(*n); },
            Self::Fun(p, r) => {
                p.variable_values(out);
                r.variable_values(out);
            },
            Self::Data(_, t) => t.variable_values(out),
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
            Type::Unit | Type::Nat | Type::Char | Type::Data(_, _) => self.fmt(f),
            Type::Var(n) => f.write_char(*map.get(n).unwrap()),
            Type::Fun(p, r) => {
                if wrap { f.write_str("(-> ")?; }
                p.display_with(f, map, true)?;
                f.write_char(' ')?;
                r.display_with(f, map, false)?;
                if wrap { f.write_char(')')?; }
                Ok(())
            },
        }
    }

    pub(crate) fn is_concrete(&self) -> bool {
        match self {
            Type::Unit | Type::Nat | Type::Char => true,
            Type::Data(_, _) => todo!("is data concrete?"),
            Type::Fun(p, r) => p.is_concrete() && r.is_concrete(),
            Type::Var(_) => false,
        }
    }

    pub(crate) fn improves(&self, t: &Type) -> bool {
        match (self, t) {
            (Type::Var(_), Type::Var(_)) => false,
            (_, Type::Var(_)) => true,
            (Type::Fun(sp, sr), Type::Fun(tp, tr)) =>
                sp.improves(tp) || sr.improves(tr),
            (x, y) if x == y => false,
            (x, y) => panic!("comparing concrete mismatch! {} {}", x, y)
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Nat => write!(f, "Nat"),
            Type::Char => write!(f, "Char"),
            Type::Data(s, _) => write!(f, "{}", s),
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
            Self::Data(arg0, _) => f.write_str(arg0),
            Self::Var(arg0) => write!(f, "t_{}", arg0),
            Self::Fun(arg0, arg1) => write!(f, "({:?} -> {:?})", arg0.as_ref(), arg1.as_ref()),
        }
    }
}
