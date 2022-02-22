#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Int,
    Var(usize),
    Fun(Box<Type>, Box<Type>),
}

