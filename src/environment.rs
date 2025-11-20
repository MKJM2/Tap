use crate::interpreter::Value;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Represents a runtime environment, which stores variables and functions.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    /// Creates a new, empty `Environment`.
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            parent: None,
        }
    }

    /// Creates a new `Environment` that is enclosed by another `Environment`.
    pub fn new_enclosed(parent: Rc<RefCell<Environment>>) -> Self {
        Environment {
            store: HashMap::new(),
            parent: Some(parent),
        }
    }

    /// Gets a value from the environment.
    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.store.get(name) {
            Some(value.clone())
        } else if let Some(parent_rc) = &self.parent {
            let parent = parent_rc.borrow();
            parent.get(name)
        } else {
            None
        }
    }

    pub fn define(&mut self, name: String, value: Value) {
        self.store.insert(name, value);
    }

    /// Sets a value in the environment, traversing up to parent scopes.
    pub fn set(&mut self, name: String, value: Value) {
        if self.store.contains_key(&name) {
            self.store.insert(name, value);
            return;
        }

        if let Some(parent_rc) = &self.parent {
            parent_rc.borrow_mut().set(name, value);
        } else {
            // If it doesn't exist in any scope, define it in the current one.
            // This is wrong for assignment, but the parser should prevent this.
            // For now, we will allow it to create a global.
            self.store.insert(name, value);
        }
    }
}
