use std::collections::HashMap;

use crate::{value::{Ident, Value}, result::*};

// use crate::builtin_fn::*;

pub type CtxtMapValue = (Value, Option<FilePos>);
pub type CtxtMap = HashMap<String, CtxtMapValue>;
#[derive(Debug, Clone)]
pub enum Context<'a> {
    FnStack {
        data: CtxtMap,
        prev: &'a Context<'a>,
    },
    Base {
        data: CtxtMap,
    }
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        // fn make_builtin(s: &str, f: fn(&Context<'_>, Vec<Token>) -> EvalResult<Value>) -> (String, CtxtMapValue) {
        //     (String::from(s), (BuiltInFn::new(s, f), None))
        // }
        Self::Base { data: 
            vec![
                // [ make_builtin("+", add)
                // , make_builtin("-", sub)
                // , make_builtin("=", eq)
                // , make_builtin("ap", ap)
                // , make_builtin("unpack", unpack)
                // , make_builtin("cons", cons)
                // , make_builtin("if", if_)
                // , make_builtin("id", id)
                // , make_builtin("quote", quote)
                // , make_builtin("list", list)
                // , make_builtin("append", append)
                // , make_builtin("assert", assert)
                // , make_builtin("match", p_match)
                // , make_builtin("dir", dir)
                ].into_iter().collect() 
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

    pub fn get(&self, k: &Ident) -> EvalResult<Value> {
        if let Some((e,_)) = self.get_data().get(&k.name) {
            Ok(e.clone())
        } else if let Some(ctxt) = &self.get_prev() {
            ctxt.get(k)
        } else {
            Err(Error::NameError(k.clone()))
        }
    }

    pub fn chain(&'a self, data: CtxtMap) -> Self {
        Self::FnStack { data, prev: &self }
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
