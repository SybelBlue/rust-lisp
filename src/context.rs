use std::collections::HashMap;

use crate::evaluator::{token::Token, value::{Ident, Value}, result::*};

use crate::builtin_fn::*;

pub type CtxtMapValue = (Value, Option<FilePos>);
pub type CtxtMap = HashMap<String, CtxtMapValue>;
#[derive(Debug, Clone)]
pub enum Context<'a> {
    FnStack {
        data: CtxtMap,
        caller: &'a Value,
        prev: &'a Context<'a>,
    },
    Base {
        data: CtxtMap,
    }
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        fn make_builtin(s: &str, f: fn(&Context<'_>, Vec<Token>) -> EvalResult<Value>) -> (String, CtxtMapValue) {
            (String::from(s), (BuiltInFn::new(s, f), None))
        }
        Self::Base { data: 
            vec![ make_builtin("+", add)
                , make_builtin("-", sub)
                , make_builtin("=", eq)
                , make_builtin("ap", ap)
                , make_builtin("unpack", unpack)
                , make_builtin("cons", cons)
                , make_builtin("if", if_)
                , make_builtin("id", id)
                , make_builtin("quote", quote)
                , make_builtin("list", list)
                , make_builtin("append", append)
                , make_builtin("assert", assert)
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

    fn get_ident(&self, k: &String) -> Option<Ident> {
        if let Some((_, op_fp)) = self.get_data().get(k) {
            op_fp.map(|fp| Ident::new(k.clone(), fp))
        } else {
            None
        }
    }

    pub fn put(&mut self, k: Ident, v: Value, allow_overwrite: bool, namespace: &Option<String>) -> EvalResult<()> {
        let key = if let Some(prefix) = namespace {
            format!("{}.{}", prefix, k.name)
        } else {
            k.name
        };

        if !allow_overwrite {
            if let Some(id) = self.get_ident(&key) {
                return Err(Error::RedefError(id, key));
            }
        }

        self.get_data_mut().insert(key, (v, Some(k.file_pos)));
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

    pub fn chain(&'a self, data: CtxtMap, caller: &'a Value) -> Self {
        Self::FnStack { data, caller, prev: &self }
    }
}
