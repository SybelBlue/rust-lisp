#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Int,
    Str,
    Type,
    Var(usize),
    Fun(Box<Type>, Box<Type>),
    Constr(Box<Constraint>, Box<Type>)
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Is(Type, Type)
}

