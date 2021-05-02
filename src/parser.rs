#[derive(Debug, PartialEq)]
pub enum Expr {
    Float(f64),
    Int(i64),
    Ident(String),
}