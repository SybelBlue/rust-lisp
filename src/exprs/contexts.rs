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
            tipe: Type::fun(Type::Nat, Type::fun(Type::Nat, Type::Nat)),
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
        // println!("New tvar: {} :: {:?}", symb, Type::Var(tvar));
        (Self::TVar { symb, tvar, base: Box::new(self) }, tvar)
    }

    pub fn put_eq(self, tvar: usize, other: Type) -> Self {
        // todo: check for conflicting restraints
        // println!("New equivalence: {:?} ~ {:?}", Type::Var(tvar), other);
        Self::VarEq(tvar, other, Box::new(self))
    }

    pub fn query(&self, t: &Type) -> Type {
        match t {
            Type::Unit | Type::Nat | Type::Char | Type::Type | Type::Data(_) => t.clone(),
            Type::Var(v) => self.query_tvar(*v),
            Type::Fun(p, r) => 
                Type::fun(self.query(p.as_ref()), self.query(r.as_ref())),
        }
    }

    pub fn query_tvar(&self, tvar: usize) -> Type {
        let goal = tvar;
        let stack = {
            let mut s = Vec::new();
            let mut curr = self;
            while let Some(next) = curr.base() {
                match curr {
                    Self::TVar { tvar, .. } if *tvar == goal => break,
                    Self::VarEq(tvar, other, _) => s.push((*tvar, other)),
                    _ => {}
                }
                curr = next;
            }
            s
        };  
        stack.into_iter()
            .fold(Type::Var(goal), 
                |curr, (tvar, sub)| curr.alpha_sub(tvar, sub))
    }
}