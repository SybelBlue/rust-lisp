pub mod checking;
pub mod contexts;

use std::{collections::{HashSet, HashMap}, fmt::{Write, Display, Formatter}};


use self::contexts::Solver;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Unit,
    Nat,
    Char,
    Data(String, Vec<Type>), // eg Data("Either", [a, Data("List", [Nat])])
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

    pub(crate) fn concretize(&self, slvr: &Solver) -> Self {
        let out = match self {
            Self::Unit | Self::Nat | Self::Char => self.clone(),
            Self::Data(nm, ts) => 
                Self::Data(nm.clone(), ts.iter().map(|t| t.concretize(slvr)).collect()),
            Self::Fun(p, r) => 
                Self::fun(p.concretize(slvr), r.concretize(slvr)),
            Self::Var(id) => slvr.query_tvar(*id),
        };
        if out == *self {
            out
        } else {
            out.concretize(slvr)
        }
    }

    pub(crate) fn variable_values(&self, out: &mut HashSet<usize>) {
        match self {
            Self::Var(n) => { out.insert(*n); }
            Self::Fun(p, r) => {
                p.variable_values(out);
                r.variable_values(out);
            }
            Self::Data(_, ts) => 
                ts.iter().for_each(|t| t.variable_values(out)),
            Self::Unit | Self::Nat | Self::Char => {}
        }
    }

    pub(crate) fn flattened(self) -> Self {
        self._flattened(&mut HashMap::new())
    }

    fn _flattened(self, bound: &mut HashMap<usize, usize>) -> Self {
        match self {
            Self::Var(n) => {
                let new = bound.len();
                Self::Var(*bound.entry(n).or_insert(new))
            }
            Self::Fun(p, r) =>
                Self::fun(p._flattened(bound), r._flattened(bound)),
            Self::Data(nm, ts) =>
                Self::Data(nm, ts.into_iter().map(|t| t._flattened(bound)).collect()),
            s@Self::Unit | s@Self::Nat | s@Self::Char => s
        }
    }

    pub(crate) fn instanced(&self, slvr: Solver) -> (Solver, Self) {
        self._instanced(&mut HashMap::new(), slvr)
    }

    fn _instanced(&self, bound: &mut HashMap<usize, usize>, slvr: Solver) -> (Solver, Self) {
        match self {
            Self::Var(n) => {
                if let Some(tvar) = bound.get(n) {
                    (slvr, Self::Var(*tvar))
                } else {
                    let (slvr, tvar) = slvr.new_tvar();
                    (slvr, Self::Var(tvar))
                }
            },
            Self::Fun(p, r) => {
                let (slvr, p) = p._instanced(bound, slvr);
                let (slvr, r) = r._instanced(bound, slvr);
                (slvr, Type::fun(p, r))
            }
            Self::Data(nm, ts) => {
                let mut slvr = slvr;
                let mut out = Vec::with_capacity(ts.len());
                for t in ts {
                    let (new, t) = t._instanced(bound, slvr);
                    slvr = new;
                    out.push(t);
                }
                (slvr, Type::Data(nm.clone(), out))
            },
            s@Self::Unit | s@Self::Nat | s@Self::Char => 
                (slvr, s.clone()),
        }
    }

    fn to_type_string(i: usize, max: usize) -> String {
        if max < 26 {
            String::from((i as u8 + 'a' as u8) as char)
        } else {
            format!("t{}", i)
        }
    }

    pub(crate) fn var_to_char_map(mut vals: Vec<usize>) -> HashMap<usize, String> {
        let max = vals.len();
        vals.sort();
        vals.into_iter()
            .enumerate()
            .map(|(i, l)| (l, Self::to_type_string(i, max)))
            .collect()
    }

    pub(crate) fn display_with(&self, f: &mut Formatter, map: &HashMap<usize, String>, wrap: bool) -> std::fmt::Result {
        match self {
            Type::Unit | Type::Nat | Type::Char | Type::Data(_, _) => self.fmt(f),
            Type::Var(n) => f.write_str(map.get(n).unwrap().as_str()),
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
            Type::Data(_, ts) => ts.iter().all(Type::is_concrete),
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
