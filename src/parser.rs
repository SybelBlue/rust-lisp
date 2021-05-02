#[derive(Debug)]
pub enum Expr {
    Float(f64),
    Int(u64),
    Ident(String),
}