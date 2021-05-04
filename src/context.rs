use std::collections::HashMap;

use crate::evaluator::{Error, EvalResult, FilePos, Ident, Value};

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

fn eq(vals: Vec<Value>) -> EvalResult<Value> {
    if let Some(u) = vals.get(0) {
        if let Some(v) = vals.get(1) {
            return Ok(Value::Int(if *u == *v { 1 } else { 0 }))
        }
    }
    Err(Error::ArgError { f_name: format!("(==)"), recieved: vals.len(), expected: 2 })
}

type CtxtMapValue = (Value, Option<FilePos>);
type CtxtMap = HashMap<String, CtxtMapValue>;
#[derive(Debug, Clone)]
pub struct Context<'a> {
    data: CtxtMap,
    prev: Option<Box<&'a Context<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        fn make_builtin(s: &str, f: fn(Vec<Value>) -> EvalResult<Value>) -> (String, CtxtMapValue) {
            (String::from(s), (Value::BuiltIn(format!("({})", s), f), None))
        }
        Self { prev: None, data: 
            vec![ make_builtin("+", add)
                , make_builtin("-", sub)
                , make_builtin("=", eq)
                ].into_iter().collect() 
            }
    }

    pub fn size(&self) -> usize {
        self.data.len() + if let Some(c) = &self.prev { c.size() } else { 0 }
    }

    fn get_ident(&self, k: &String) -> Option<Ident> {
        if let Some((_, op_fp)) = self.data.get(k) {
            op_fp.map(|fp| Ident::new(k.clone(), fp))
        } else {
            None
        }
    }

    pub fn put(&mut self, k: Ident, v: Value, allow_overwrite: bool) -> EvalResult<()> {
        if !allow_overwrite {
            if let Some(id) = self.get_ident(&k.name) {
                return Err(Error::RedefError(id, k.name));
            }
        }
        self.data.insert(k.name, (v, Some(k.file_pos)));
        Ok(())
    }

    pub fn get(&self, k: &Ident) -> EvalResult<Value> {
        if let Some((e,_)) = self.data.get(&k.name) {
            Ok(e.clone())
        } else if let Some(ctxt) = &self.prev {
            ctxt.get(k)
        } else {
            Err(Error::NameError(k.clone()))
        }
    }

    pub fn chain(&'a self, data: CtxtMap) -> Self {
        Self { data, prev: Some(Box::new(self)) }
    }
}
