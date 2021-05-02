use std::collections::{HashMap, VecDeque};

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
    Fn(VecDeque<String>, Box<Expr>),
}

// #[derive(Debug)]
// pub struct Context {
//     data: Vec<HashMap<String, Value>>,
// }

// impl Context {
//     pub fn get(&self, k: &String) -> Result<Value, String> {
//         for m in self.data.iter().rev() {
//             if let Some(v) = m.get(k) {
//                 return Ok(v.clone())
//             }
//         }
//         Err(format!("NameError: \"{}\" not found", k))
//     }

//     // pub fn push(&mut self)
// }

#[derive(Debug)]
pub struct Context<'a> {
    data: HashMap<String, Value>,
    prev: Option<Box<&'a Context<'a>>>,
}

impl<'a> Context<'a> {
    pub fn get(&self, k: &String) -> Result<Value, String> {
        if let Some(e) = self.data.get(k) {
            Ok(e.clone())
        } else if let Some(ctxt) = &self.prev {
            ctxt.get(k)
        } else {
            Err(format!("NameError: {}", k))
        }
    }

    pub fn chain(&'a self, data: HashMap<String, Value>) -> Self {
        Self { data, prev: Some(Box::new(self)) }
    }
}

impl Expr {
    pub fn eval(&self, ctxt: &Context) -> Result<Value, String> {
        match self {
            Expr::Lit(v) => Ok(v.clone()),
            Expr::Ident(id) => ctxt.get(id),
            Expr::Form(h, tail) => {
                match ctxt.get(h)? {
                    Value::Fn(params, body) => {
                        if tail.len() != params.len() {
                            return Err(format!("Not enough args provided to {}: expected {}, got {}", h, params.len(), tail.len()))
                        }
                        let mut args = Vec::new();
                        for a in tail {
                            args.push(a.eval(ctxt)?);
                        }
                        let next = ctxt.chain(params.into_iter().zip(args).collect());
                        body.as_ref().eval(&next)
                    },
                    v => Ok(v),
                }
            },
        }
    }
}
