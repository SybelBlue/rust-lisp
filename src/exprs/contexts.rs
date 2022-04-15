use std::collections::{HashSet, HashMap};

use super::types::{Type, TypeError};

pub type Ident = String;
pub type QualifiedIdent = String;

pub type EquivalenceClass = HashSet<Type>;

#[derive(Debug)]
pub struct FlatTypeContext {
    equiv_classes: Vec<EquivalenceClass>,
    bound: HashMap<Ident, Type>,
}

impl From<&TypeContext> for FlatTypeContext {
    fn from(ctxt: &TypeContext) -> Self {
        Self { equiv_classes: ctxt.equiv_classes(), bound: ctxt.as_hash() }
    }
}

#[derive(Debug, Clone)]
pub enum TypeContext {
    Empty,
    Entry {
        symb: Ident,
        tipe: Type,
        base: Box<TypeContext>,
    },
    TVar {
        symb: Ident,
        tvar: usize,
        base: Box<TypeContext>,
    },
    VarEq(usize, Type, Box<TypeContext>),
}

impl TypeContext {
    pub fn new() -> Self {
        Self::Entry {
            symb: String::from("+"),
            tipe: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)),
            base: Box::new(Self::Empty),
        }
    }

    pub fn get<'a>(&'a self, key: &Ident) -> Option<Type> {
        match self {
            Self::Entry { symb, tipe, ..} if symb == key =>
                    Some(tipe.clone()),
            Self::TVar { symb, tvar, ..} if symb == key =>
                    Some(Type::Var(*tvar)),
            ctxt => ctxt.base().and_then(|b| b.get(key)),
        }
    }

    fn base(&self) -> Option<&TypeContext> {
        match self {
            Self::Empty => None,
            Self::Entry { base, .. } 
                | Self::TVar { base, .. }
                | Self::VarEq(_, _, base) 
                => Some(base.as_ref()),
        }
    }

    fn tvar(&self) -> usize {
        if let Self::TVar { tvar, .. } = self {
            *tvar
        } else {
            self.base().map(|b| b.tvar()).unwrap_or(0)
        }
    }

    pub fn put(self, symb: Ident, tipe: Type) -> Self {
        Self::Entry { symb, tipe, base: Box::new(self) }
    }

    pub fn put_new_tvar(self, symb: Ident) -> (Self, usize) {
        let tvar = self.tvar() + 1;
        (Self::TVar { symb, tvar, base: Box::new(self) }, tvar)
    }

    pub fn put_eq(self, tvar: usize, other: Type) -> Self {
        // todo: check for conflicting restraints
        Self::VarEq(tvar, other, Box::new(self))
    }

    pub fn concretize(self, id: usize) -> Result<(Type, Self), TypeError<'static>> {
        let cls = self.equivalences(id);
        let conc: Vec<&Type> = cls.iter().filter(|t| t.is_concrete()).collect();
        Ok((match conc.as_slice() {
            &[] => cls.into_iter().max().unwrap(),
            &[t] => t.clone(),
            &[s, t, ..] => return Err(TypeError::BadEquivalence(s.clone(), t.clone()))
        }, self))
    }

    pub fn flatten(&self) -> FlatTypeContext {
        FlatTypeContext::from(self)
    }

    fn as_hash(&self) -> HashMap<Ident, Type> {
        self.unpack()
            .into_iter()
            .filter_map(|ctxt| {
                match ctxt {
                    TypeContext::Entry { symb, tipe, .. } => Some((symb.clone(), tipe.clone())),
                    TypeContext::TVar { symb, tvar, .. } => Some((symb.clone(), Type::Var(*tvar))),
                    _ => None,
                }
            })
            .collect()
    }

    pub(crate) fn equiv_classes(&self) -> Vec<EquivalenceClass> {
        let mut eqs: Vec<EquivalenceClass> = Vec::new();
        for n in 1..=self.tvar() {
            if !eqs.iter().any(|e| e.contains(&Type::Var(n))) {
                eqs.push(self.equivalences(n));
            }
        }
        eqs
    }

    pub(crate) fn equivalences(&self, tvar: usize) -> EquivalenceClass {
        let mut out = HashSet::new();
        for ctxt in self.unpack().into_iter().rev() {
            match ctxt {
                Self::VarEq(id, tipe, _) if *id == tvar || out.contains(&Type::Var(*id)) => { 
                    out.insert(tipe.clone()); 
                }
                Self::VarEq(id, tipe, _) if &Type::Var(tvar) == tipe => {
                    out.insert(Type::Var(*id));
                }
                _ => {}
            }
        }
        out.insert(Type::Var(tvar));
        out

    }

    /// Returns an inside-out vec of this context
    pub fn unpack(&self) -> Vec<&Self> {
        let mut slf = vec![self];
        let mut curr = self;
        while let Some(next) = curr.base() {
            slf.push(next);
            curr = next;
        }
        slf
    }
}