#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Ident(String),
    Form(Box<Expr>, Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
}

impl Expr {
    pub fn eval(&self) -> Value {
        todo!()
    }
}
