use std::collections::HashMap;

use crate::{value::{Ident, Value}, result::*};

use crate::builtin_fn::*;

pub type CtxtMapValue = (Value, Option<FilePos>);
pub type CtxtMap = HashMap<String, CtxtMapValue>;
#[derive(Debug, Clone)]
pub struct Context {
    data: CtxtMap
}

impl Context {
    pub fn new() -> Self {
        // fn make_builtin(s: &str, f: fn(&Context, Vec<Token>) -> EvalResult<Value>) -> (String, CtxtMapValue) {
        //     (String::from(s), (BuiltInFn::new(s, f), None))
        // }
        Self { data: 
            vec![]
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
                // ]
                .into_iter().collect() 
            }
    }

    fn get_data(&self) -> &CtxtMap {
        &self.data
    }

    fn get_data_mut(&mut self) -> &mut CtxtMap {
        &mut self.data
    }

    fn get_prev(&self) -> Option<&Self> {
        unimplemented!()
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

    pub fn chain(&self, data: CtxtMap) -> Self {
        unimplemented!()
    }

    pub fn chain_new(&self, capacity: usize) -> Self {
        self.chain(CtxtMap::with_capacity(capacity))
    }

    pub fn dir(&self) -> Vec<&String> {
        let d = self.get_data().keys();
        if let Some(prev) = self.get_prev() {
            d.chain(prev.dir()).collect()
        } else {
            d.collect()
        }
    }
}
