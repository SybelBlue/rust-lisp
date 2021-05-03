use std::collections::HashMap;

use crate::evaluator::{Ident, Error, EvalResult, Value};

#[derive(Debug, Clone)]
pub struct Context<'a> {
    data: HashMap<String, Value>,
    prev: Option<Box<&'a Context<'a>>>,
}

fn add(vals: Vec<Value>) -> EvalResult<Value> {
    use Value::*;

    let out: &mut Result<i64, f64> = &mut Ok(0);
    for v in vals {
        match v {
            Int(n) => match out {
                Ok(x) => *x += n,
                Err(x) => *x += n as f64,
            },
            Float(n) => match out {
                Ok(x) => *out = Err(n + *x as f64),
                Err(x) => *x += n,
            },
            x => return Err(Error::ValueError(x, format!("(+) takes only numeric arguments"))),
        }
    }

    Ok(match *out {
        Ok(x) => Int(x),
        Err(x) => Float(x),
    })
}

fn sub(vals: Vec<Value>) -> EvalResult<Value> {
    use Value::*;
    if let Some((h, t)) = vals.split_first() {
        if t.len() > 0 {
            return match *h {
                Int(n) => match add(Vec::from(t))? {
                    Int(x) => Ok(Int(n - x)),
                    Float(x) => Ok(Float(n as f64 - x)),
                    x => Err(Error::ValueError(x, format!("Addition returned non-numeric"))),
                },
                Float(n) => match add(Vec::from(t))? {
                    Int(x) => Ok(Float(n - x as f64)),
                    Float(x) => Ok(Float(n - x)),
                    x => Err(Error::ValueError(x, format!("Addition returned non-numeric"))),
                },
                _ => Err(Error::ValueError(h.clone(), format!("(-) takes only numeric arguments"))),
            }
        }
    }
    Err(Error::ArgError { f_name: format!("(-)"), recieved: vals.len(), expected: 2 })
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        fn make_builtin(s: &str, f: fn(Vec<Value>) -> EvalResult<Value>) -> (String, Value) {
            (String::from(s), Value::BuiltIn(format!("({})", s), f)) 
        }
        Self { prev: None, data: vec![make_builtin("+", add), make_builtin("-", sub)].into_iter().collect() }
    }

    pub fn size(&self) -> usize {
        self.data.len() + if let Some(c) = &self.prev { c.size() } else { 0 }
    }

    fn get_ident(&self, k: &String) -> Option<Ident> {
        todo!()
    }

    pub fn put(&mut self, k: String, v: Value, allow_overwrite: bool) -> EvalResult<()> {
        if !allow_overwrite {
            if let Some(id) = self.get_ident(&k) {
                return Err(Error::RedefError(id, k));
            }
        }
        self.data.insert(k, v.clone());
        Ok(())
    }

    pub fn get(&self, k: &String) -> EvalResult<Value> {
        if let Some(e) = self.data.get(k) {
            Ok(e.clone())
        } else if let Some(ctxt) = &self.prev {
            ctxt.get(k)
        } else {
            Err(Error::NameError(k.clone()))
        }
    }

    pub fn chain(&'a self, data: HashMap<String, Value>) -> Self {
        Self { data, prev: Some(Box::new(self)) }
    }
}
