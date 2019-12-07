use std::fmt;
use std::collections::{HashMap};

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Null,
    Integer(i64),
    String(String),
    Boolean(bool),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Integer(i) => i.fmt(f),
            Object::Boolean(b) => b.fmt(f),
            Object::String(s) => s.fmt(f),
        }
    }
}


#[derive(Clone, Debug, Default, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_enclosed(outer: Self) -> Self {
        let mut env = Self::new();
        env.outer = Some(Box::new(outer));
        env
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        match (self.store.get(name), &self.outer) {
            (Some(obj), _) => Some(obj),
            (None, Some(outer)) => outer.get(name),
            (None, _) => None,
        }
    }

    pub fn set(&mut self, name: String, obj: &Object) -> Object {
        self.store.insert(name, obj.clone());
        obj.clone()
    }
}