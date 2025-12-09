use crate::interpreter::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
struct Scope {
    values: HashMap<String, Value>,
    enclosing: Option<Environment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    state: Rc<RefCell<Scope>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            state: Rc::new(RefCell::new(Scope {
                values: HashMap::new(),
                enclosing: None,
            })),
        }
    }

    pub fn enclose(&self) -> Self {
        Environment {
            state: Rc::new(RefCell::new(Scope {
                values: HashMap::new(),
                enclosing: Some(self.clone()),
            })),
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.state.borrow_mut().values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        let state = self.state.borrow();
        if let Some(val) = state.values.get(name) {
            // --- ADD THIS DEBUG BLOCK ---
            if let Value::List(vec) = val {
                if vec.len() > 1000 && vec.len() % 1000 == 0 {
                    println!(
                        "PERF WARNING: Deep cloning list of size {} from variable '{}'",
                        vec.len(),
                        name
                    );
                }
            }
            // ----------------------------
            return Some(val.clone());
        }

        if let Some(enclosing) = &state.enclosing {
            return enclosing.get(name);
        }

        None
    }

    fn update_if_exists(&self, name: &str, value: Value) -> bool {
        let mut state = self.state.borrow_mut();
        if state.values.contains_key(name) {
            state.values.insert(name.to_string(), value);
            return true;
        }
        if let Some(enclosing) = &state.enclosing {
            return enclosing.update_if_exists(name, value);
        }
        false
    }

    pub fn set(&mut self, name: &str, value: Value) -> bool {
        if self.update_if_exists(name, value.clone()) {
            return true;
        }
        self.define(name.to_string(), value);
        true
    }
}
