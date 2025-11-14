use crate::interpreter::Value;
use std::collections::HashMap;

/// Represents a runtime environment, which stores variables and functions.
#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    store: HashMap<String, Value>,
    parent: Option<Box<Environment>>,
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
    pub fn new_enclosed(parent: &Environment) -> Self {
        Environment {
            store: HashMap::new(),
            parent: Some(Box::new(parent.clone())),
        }
    }

    /// Gets a value from the environment.
    pub fn get(&self, name: &str) -> Option<&Value> {
        if let Some(value) = self.store.get(name) {
            Some(value)
        } else if let Some(parent) = &self.parent {
            parent.get(name)
        } else {
            None
        }
    }

    /// Sets a value in the environment.
    pub fn set(&mut self, name: String, value: Value) {
        self.store.insert(name, value);
    }
}