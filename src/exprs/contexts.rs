use super::types::Type;

pub type Ident = String;
pub type QualifiedIdent = String;

#[derive(Debug, Clone)]
pub enum TypeContext {
    Empty,
    Entry {
        symb: Ident,
        tipe: Type,
        base: Box<TypeContext>,
        tvar: usize,
    }
}

impl TypeContext {
    pub fn new() -> Self {
        Self::Entry {
            symb: String::from("+"),
            tipe: Type::fun(Type::Int, Type::fun(Type::Int, Type::Int)),
            tvar: 0,
            base: Box::new(Self::Empty),
        }
    }

    pub fn get<'a>(&'a self, key: &Ident) -> Option<&'a Type> {
        match self {
            Self::Empty => None,
            Self::Entry { symb, tipe, base, ..} => {
                if symb == key {
                    Some(tipe)
                } else {
                    base.as_ref().get(key)
                }
            }
        }
    }

    fn tvar(&self) -> usize {
        if let Self::Entry { tvar, .. } = self {
            *tvar
        } else {
            0
        }
    }

    pub fn put(self, symb: Ident, tipe: Type) -> Self {
        let tvar = self.tvar();
        Self::Entry { symb, tipe, base: Box::new(self), tvar }
    }

    pub fn put_new_tvar(self, symb: Ident) -> (Self, Type) {
        let tvar = self.tvar();
        let tipe = Type::Var(format!("{}_{}", symb, tvar));
        let tvar = tvar + 1;
        (Self::Entry { symb, tipe: tipe.clone(), base: Box::new(self), tvar }, tipe)
    }
}