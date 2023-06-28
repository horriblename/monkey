use parser::ast::{self, representation::StringRepr};
use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use crate::{builtins, error::EResult};

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    String,
    Null,
    Array,
    ReturnValue,
    Function,
    BuiltinFunc,
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
#[derive(Debug, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Null,
    Array(Array),
    // cloning Rc might bite me in the ass some day
    ReturnValue(ObjectRc),
    Function(Function),
    BuiltinFunc(Builtin),
}

pub type ObjectRc = Rc<RefCell<Object>>;

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Int(val) => format!("{}", val),
            Self::Bool(val) => format!("{}", val),
            Self::String(s) => s.clone(),
            Self::Null => "null".to_string(),
            Self::Array(arr) => {
                let elements = arr
                    .elements
                    .iter()
                    .map(|el| el.borrow().inspect())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("[{}]", elements)
            }
            Self::ReturnValue(val) => val.borrow().inspect(),
            Self::Function(val) => {
                let params = val
                    .parameters
                    .iter()
                    .map(|ident| ident.string_repr())
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("fn({}) {}", params, val.body.string_repr())
            }
            Self::BuiltinFunc(_) => "builtin function".to_string(),
        }
    }

    // TODO: is there no better way? :(
    pub fn type_(&self) -> ObjectType {
        match self {
            Self::Int(_) => ObjectType::Integer,
            Self::Bool(_) => ObjectType::Boolean,
            Self::String(_) => ObjectType::String,
            Self::Null => ObjectType::Null,
            Self::Array(_) => ObjectType::Array,
            Self::ReturnValue(_) => ObjectType::ReturnValue,
            Self::Function(_) => ObjectType::Function,
            Self::BuiltinFunc(_) => ObjectType::BuiltinFunc,
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x == y,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Null, Self::Null) => true,
            (Self::String(x), Self::String(y)) => x == y,
            (Self::ReturnValue(x), Self::ReturnValue(y)) => x == y,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<ObjectRc>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    // TODO: remove Option
    pub env: Environment,
}

#[derive(Clone)]
pub struct Builtin {
    func: &'static builtins::BuiltinFuncSignature,
}

impl Builtin {
    pub fn from_func_name(name: &str) -> Option<Builtin> {
        builtins::get_builtin(name).map(|func| Builtin { func })
    }

    pub fn call(&self, args: &Vec<ObjectRc>) -> EResult<ObjectRc> {
        (self.func)(args)
    }
}

impl Debug for Builtin {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin function")
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Rc<RefCell<Object>>>,
}

// #[derive(Debug, Clone)]
// TODO: enforce rule of always having at least one Environment on stack
pub struct EnvStack {
    stack: Vec<Environment>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    // I'm debating removing this function
    pub fn new_enclosed_environment(outer: &Self) -> Self {
        outer.clone()
    }

    pub fn get(&self, name: &str) -> Option<Rc<RefCell<Object>>> {
        self.store.get(name).map(|var| var.clone())
    }

    pub fn set(&mut self, name: String, val: Rc<RefCell<Object>>) -> Rc<RefCell<Object>> {
        self.store.insert(name, val.clone());
        val
    }
}

impl EnvStack {
    pub fn new() -> Self {
        EnvStack {
            stack: vec![Environment::new()],
        }
    }

    pub fn new_enclosed_environment(&mut self) -> &mut Self {
        self.stack.push(Environment::new());
        self
    }

    pub fn pop(&mut self) -> Environment {
        debug_assert!(self.stack.len() >= 2);
        self.stack.pop().expect("empty EnvStack!")
    }

    pub fn get(&self, name: &str) -> Option<Rc<RefCell<Object>>> {
        self.stack.iter().rev().find_map(|env| env.get(name))
    }

    pub fn set(&mut self, name: String, val: Rc<RefCell<Object>>) -> Rc<RefCell<Object>> {
        self.stack
            .last_mut()
            .expect("empty EnvStack!")
            .set(name, val)
    }

    pub fn peek(&self) -> &Environment {
        self.stack.last().expect("empty EnvStack!")
    }
}
