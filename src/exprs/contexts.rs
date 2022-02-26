use std::collections::HashSet;

use super::types::{Type, TypeError};

pub type Ident = String;
pub type QualifiedIdent = String;

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

    pub fn put_eq(self, tvar: usize, other: Type) -> Option<Self> {
        // todo: check for conflicting restraints
        Some(Self::VarEq(tvar, other, Box::new(self)))
    }

    pub fn concretize(self, id: usize) -> Result<(Self, Type), TypeError<'static>> {
        println!("{:?}", self);
        let mut to_check = vec![id];
        let mut checked = HashSet::new();
        let mut last = id;
        let mut found = None;
        while let Some(tvar) = to_check.pop() {
            last = tvar;
            checked.insert(tvar);
            for ctxt in TypeContextIter::new(&self) {
                match ctxt {
                    TypeContext::Entry { .. } | TypeContext::Empty => {},
                    TypeContext::TVar { tvar, .. } => 
                        if *tvar == id { break; },
                    TypeContext::VarEq(tvar, tipe, _) => {
                        if *tvar == id {
                            match tipe {
                                Type::Var(id2) => {
                                    if !checked.contains(id2) {
                                        to_check.push(*id2);
                                    }
                                },
                                Type::Fun(_, _) => todo!("impl concretization for fn"),
                                tipe => {
                                    let tipe = tipe.clone();
                                    match found {
                                        None => { found = Some(tipe); },
                                        Some(f) if f == tipe => return Ok((self, tipe)),
                                        Some(f) => return Err(TypeError::BadEquivalence(f, tipe)),
                                    }
                                }
                            }
                        }
                    },
                }
            }
        }
        Ok((self, Type::Var(last)))
    }
}

struct TypeContextIter<'a>(Option<&'a TypeContext>);

impl<'a> TypeContextIter<'a> {
    pub fn new(ctxt: &'a TypeContext) -> Self {
        Self(Some(ctxt))
    }
}

impl<'a> std::iter::Iterator for TypeContextIter<'a> {
    type Item = &'a TypeContext;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(curr) = self.0 {
            self.0 = curr.base();
            Some(curr)
        } else {
            None
        }
    }
}