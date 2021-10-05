use std::collections::HashMap;

use crate::{builtin_fn::{BuiltInFn, add}, expr::Expr, result::*, rtype::Type, value::{Ident, Value}};

// use crate::builtin_fn::*;

pub type GenCtxtMapVal<T> = (T, Option<FilePos>);
pub type GenCtxtMap<T> = HashMap<String, GenCtxtMapVal<T>>;
pub type CtxtMapVal = GenCtxtMapVal<Value>;
pub type CtxtMap = GenCtxtMap<Value>;
pub type TypeMapVal = GenCtxtMapVal<Type>;
pub type TypeMap = GenCtxtMap<Type>;

#[derive(Debug, Clone)]
pub enum Context<'a> {
    FnStack {
        deferred_types: GenCtxtMap<Type>,
        data: CtxtMap,
        prev: &'a Context<'a>,
    },
    Base {
        data: CtxtMap,
    }
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        fn make_builtin(s: &str, tp: Type, f: fn(&Context<'_>, Vec<Expr>) -> EvalResult<Value>) -> (String, CtxtMapVal) {
            (String::from(s), (BuiltInFn::new_val(s, tp, f), None))
        }
        Self::Base { data: 
            vec![
                make_builtin(
                    "+", 
                    Type::Arrow(Box::new(Type::Int), Box::new(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)))), 
                    add)].into_iter().collect() 
            }
    }

    fn get_data(&self) -> &CtxtMap {
        match self {
            Self::Base { data, .. } => data,
            Self::FnStack { data, .. } => data,
        }
    }

    fn get_data_mut(&mut self) -> &mut CtxtMap {
        match self {
            Self::Base { data, .. } => data,
            Self::FnStack { data, .. } => data,
        }
    }

    fn get_prev(&self) -> Option<&Self> {
        if let Self::FnStack { prev, .. } = self {
            Some(*prev)
        } else {
            None
        }
    }

    pub fn size(&self) -> usize {
        self.get_data().len() + if let Some(c) = &self.get_prev() { c.size() } else { 0 }
    }

    pub fn put(&mut self, k: Ident, v: Value) -> EvalResult<()> {
        self.get_data_mut().insert(k.name, (v, Some(k.file_pos)));
        Ok(())
    }

    pub fn get(&'a self, k: &Ident) -> EvalResult<&'a Value> {
        if let Some((e,_)) = self.get_data().get(&k.name) {
            Ok(e)
        } else if let Some(ctxt) = &self.get_prev() {
            ctxt.get(k)
        } else {
            Err(Error::NameError(k.clone()))
        }
    }
    
    pub fn get_type(&'a self, k: &Ident) -> EvalResult<&'a Type> {
        if let Self::FnStack { deferred_types, .. } = self {
            if let Some((t, _)) = deferred_types.get(&k.name) {
                return Ok(t);
            }
        }
        if let Some(other) = self.get_prev() {
            if let Ok(x) = other.get_type(k) {
                return Ok(x);
            }
        }
        self.get(k).map(Value::get_type)
    }

    pub fn chain(&'a self, data: CtxtMap) -> Self {
        Self::FnStack { data, deferred_types: HashMap::new(), prev: &self }
    }

    pub fn chain_new(&'a self, capacity: usize) -> Self {
        self.chain(CtxtMap::with_capacity(capacity))
    }

    pub fn dir(&'a self) -> Vec<&'a String> {
        let d = self.get_data().keys();
        if let Some(prev) = self.get_prev() {
            d.chain(prev.dir()).collect()
        } else {
            d.collect()
        }
    }
}
