use std::collections::{HashMap, VecDeque};

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Lit(Value),
    Ident(String),
    Form(String, Vec<Expr>),
    Def(String, Box<Expr>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Int(i64),
    Float(f64),
    Fn(VecDeque<String>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Context<'a> {
    data: HashMap<String, Value>,
    prev: Option<Box<&'a Context<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self { prev: None, data: HashMap::new() }
    }

    pub fn put(&mut self, k: String, v: Value, allow_overwrite: bool) -> Result<Value, String> {
        if !allow_overwrite && self.data.contains_key(&k) {
            Err(format!("NameError: {} already defined in scope", k))
        } else {
            self.data.insert(k, v.clone());
            Ok(v)
        }
    }

    pub fn get(&self, k: &String) -> Result<Value, String> {
        if let Some(e) = self.data.get(k) {
            Ok(e.clone())
        } else if let Some(ctxt) = &self.prev {
            ctxt.get(k)
        } else {
            Err(format!("NameError: {} not defined in scope", k))
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
                            return Err(format!(
                                "ArgError: Not enough args provided to {}: expected {}, got {}", h, params.len(), tail.len()))
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
            Expr::Def(n, _) => Err(format!("Tried to define in an immutable scope {}", n)),
        }
    }

    pub fn exec(&self, ctxt: &mut Context, allow_overwrite: bool) -> Result<Value, String> {
        match self {
            Expr::Def(n, body) => 
                ctxt.put(n.clone(), body.as_ref().eval(&ctxt)?, allow_overwrite),
            _ => self.eval(&ctxt),
        }
    }
}

pub(crate) fn exec_batch(exprs: Vec<Expr>) -> (Vec<Result<Value, String>>, Context<'static>) {
    let mut ctxt = Context::new();
    (exprs.iter().map(|e| e.exec(&mut ctxt, false)).collect(), ctxt)
}
