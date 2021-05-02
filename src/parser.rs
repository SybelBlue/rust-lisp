use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Ident(String),
    Form(String, Vec<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Fn(Vec<String>, Box<Expr>),
}

#[derive(Debug)]
pub struct Context {
    data: HashMap<String, Expr>,
    prev: Option<Box<Context>>,
}

impl Context {
    pub fn get(&self, k: &String) -> Result<Expr, String> {
        if let Some(e) = self.data.get(k) {
            Ok(e.clone())
        } else if let Some(ctxt) = &self.prev {
            ctxt.get(k)
        } else {
            Err(format!("NameError: {}", k))
        }
    }
}

impl Expr {
    pub fn eval(&self, ctxt: &Context) -> Result<Value, String> {
        match self {
            Expr::Lit(v) => Ok(v.clone()),
            Expr::Ident(id) => ctxt.get(id)?.eval(ctxt),
            Expr::Form(h, tail) => todo!(),
        }
    }
}
