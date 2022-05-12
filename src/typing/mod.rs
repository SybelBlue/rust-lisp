pub mod infer;
pub mod subst;
pub mod scheme;
pub mod contraint;

use std::{collections::{HashSet, HashMap}, fmt::{Write, Debug, Display, Formatter}};

use self::subst::{Substitutable, Subst};

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Type {
    Data(String, Vec<Type>), // eg Data("Either", [a, Data("List", [Nat])])
    Var(usize),
    Fun(Box<Type>, Box<Type>),
}

impl Type {
    pub fn fun(p: Self, b: Self) -> Self {
        Self::Fun(Box::new(p), Box::new(b))
    }

    pub(crate) fn simple(s: &str) -> Self {
        Self::Data(String::from(s), Vec::with_capacity(0)) 
    }

    pub(crate) fn unit() -> Self { Self::simple("Unit") }
    pub(crate) fn nat() -> Self { Self::simple("Nat") }
    pub(crate) fn char() -> Self { Self::simple("Char") }

    pub(crate) fn normalize(&self, map: &mut HashMap<usize, usize>) -> Self {
        match self {
            Type::Data(d, ts) => 
                Type::Data(d.clone(), ts.into_iter().map(|t| t.normalize(map)).collect()),
            Type::Var(k) => {
                let n = map.len();
                Type::Var(*map.entry(*k).or_insert(n))
            }
            Type::Fun(p, b) => 
                Type::fun(p.normalize(map), b.normalize(map))
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
            Type::Data(_, _) => Display::fmt(self, f),
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
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Data(s, ts) => {
                if !ts.is_empty() { f.write_char('(')?; }
                f.write_str(s)?;
                if !ts.is_empty() { 
                    let mut vals = HashSet::new();
                    self.variable_values(&mut vals);
                    self.display_with(f, &Self::var_to_char_map(vals.into_iter().collect()), true)?;
                    f.write_char(')')?; 
                }
                Ok(())
            }
            Self::Fun(_, _) => {
                let mut vals = HashSet::new();
                self.variable_values(&mut vals);
                self.display_with(f, &Self::var_to_char_map(vals.into_iter().collect()), true)
            },
            Self::Var(_) => f.write_char('a'),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Data(s, ts) => {
                if !ts.is_empty() { f.write_char('(')?; }
                f.write_str(s)?;
                for t in ts {
                    write!(f, " {:?}", t)?;
                }
                if !ts.is_empty() { f.write_char(')')?; }
                Ok(())
            }
            Self::Var(arg0) => write!(f, "t_{}", arg0),
            Self::Fun(arg0, arg1) => write!(f, "({:?} -> {:?})", arg0.as_ref(), arg1.as_ref()),
        }
    }
}

impl Substitutable for Type {
    fn apply(&self, sub: &Subst) -> Self {
        match self {
            data@Type::Data(_, _) => 
                data.clone(),
            default@Type::Var(k) => 
                sub.get_default(k, default).clone(),
            Type::Fun(p, r) => 
                Type::fun(p.apply(sub), r.apply(sub)),
        }
    }

    fn ftv(&self, used: &mut HashSet<usize>) {
        match self {
            Type::Data(_, ts) => 
                ts.iter().for_each(|t| t.ftv(used)),
            Type::Var(x) => {
                used.insert(*x);
            }
            Type::Fun(p, r) => {
                p.ftv(used);
                r.ftv(used);
            }
        }
    }
}
