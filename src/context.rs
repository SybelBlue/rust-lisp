use std::collections::HashMap;

use crate::parser::Value;

#[derive(Debug, Clone)]
pub struct Context<'a> {
    data: HashMap<String, Value>,
    prev: Option<Box<&'a Context<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self { prev: None, data: HashMap::new() }
    }

    pub fn put(&mut self, k: String, v: Value, allow_overwrite: bool) -> Result<(), String> {
        if !allow_overwrite && self.data.contains_key(&k) {
            Err(format!("NameError: {} already defined in scope", k))
        } else {
            self.data.insert(k, v.clone());
            Ok(())
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
