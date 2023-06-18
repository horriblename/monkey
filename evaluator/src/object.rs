use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    ReturnValue,
}

// In the book, Object is used as an interface:
//
// ```go
// type ObjectType string
// type Object interface {
//     Type() ObjectType
//     Inspect() string
// }
// ```
//
// reflection is used to differentiate between Objects
#[derive(Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Null,
    ReturnValue(ObjectRc),
}

pub type ObjectRc = Rc<RefCell<Object>>;

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Int(val) => format!("{}", val),
            Self::Bool(val) => format!("{}", val),
            Self::Null => "null".to_string(),
            Self::ReturnValue(val) => val.borrow().inspect(),
        }
    }

    // TODO: is there no better way? :(
    pub fn type_(&self) -> ObjectType {
        match self {
            Self::Int(_) => ObjectType::Integer,
            Self::Bool(_) => ObjectType::Boolean,
            Self::Null => ObjectType::Null,
            Self::ReturnValue(_) => ObjectType::ReturnValue,
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x == y,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Null, Self::Null) => true,
            (Self::ReturnValue(x), Self::ReturnValue(y)) => x == y,
            _ => false,
        }
    }
}

pub struct Environment {
    store: HashMap<String, Rc<RefCell<Object>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<RefCell<Object>>> {
        self.store.get(name).map(|var| var.clone())
    }

    pub fn set(&mut self, name: String, val: Rc<RefCell<Object>>) -> Rc<RefCell<Object>> {
        self.store.insert(name, val.clone());
        val
    }
}
