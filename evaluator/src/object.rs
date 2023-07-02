use parser::ast::{self, representation::StringRepr};
use std::{
    cell::RefCell,
    collections::{hash_map, HashMap},
    fmt::Debug,
    rc::Rc,
};

use crate::{builtins, error::EResult};

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    String,
    Null,
    Array,
    Hash,
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
    Hash(Hash),
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
            Self::Hash(h) => {
                let elements = h
                    .pairs
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.inspect(), v.borrow().inspect()))
                    .collect::<Vec<_>>()
                    .join(", ");

                format!("{{{}}}", elements)
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
            Self::Hash(_) => ObjectType::Hash,
            Self::ReturnValue(_) => ObjectType::ReturnValue,
            Self::Function(_) => ObjectType::Function,
            Self::BuiltinFunc(_) => ObjectType::BuiltinFunc,
        }
    }
}

impl Eq for Object {}
impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(x), Self::Int(y)) => x == y,
            (Self::Bool(x), Self::Bool(y)) => x == y,
            (Self::Null, Self::Null) => true,
            (Self::String(x), Self::String(y)) => x == y,
            // (Self::ReturnValue(x), Self::ReturnValue(y)) => x == y,
            _ => false,
        }
    }
}

impl std::hash::Hash for Object {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Object::Int(n) => n.hash(state),
            Object::Bool(n) => n.hash(state),
            Object::String(s) => s.hash(state),
            Object::Null => 0.hash(state),
            // O(n) operation defeats the point of hashing?
            Object::Array(arr) => arr.elements.iter().for_each(|el| el.borrow().hash(state)),
            // FIXME: HashMap.iter() is NOT guaranteed order!
            Object::Hash(h) => h.pairs.iter().for_each(|(k, v)| {
                k.hash(state);
                v.borrow().hash(state)
            }),
            Object::ReturnValue(_) => todo!(),
            Object::Function(_) => todo!(),
            Object::BuiltinFunc(_) => todo!(),
        };
    }
}

#[derive(Debug, Clone)]
pub struct Array {
    pub elements: Vec<ObjectRc>,
}

#[derive(Debug, Clone)]
pub struct Hash {
    pairs: HashMap<Object, ObjectRc>,
}

impl Hash {
    pub fn new() -> Self {
        Self {
            pairs: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: Object, value: ObjectRc) {
        use Object::*;
        match key {
            key @ (Int(_) | Bool(_) | String(_) | Null) => {
                self.pairs.insert(key, value);
            }
            _ => todo!("error handling: unhashable key"),
        }
    }

    pub fn get(&self, key: &Object) -> Option<ObjectRc> {
        use Object::*;
        match key {
            key @ (Int(_) | Bool(_) | String(_) | Null) => self.pairs.get(key).map(Rc::clone),
            _ => todo!("error handling: unhashable key"),
        }
    }

    pub fn iter(&self) -> hash_map::Iter<Object, ObjectRc> {
        self.pairs.iter()
    }

    pub fn len(&self) -> usize {
        self.pairs.len()
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    pub env: Rc<RefCell<EnvStack>>,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|ident| ident.string_repr())
            .collect::<Vec<_>>()
            .join(", ");

        write!(f, "fn({}) {{...}}", params)
    }
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
struct Environment {
    store: HashMap<String, Rc<RefCell<Object>>>,
}

// #[derive(Debug, Clone)]
// TODO: enforce rule of always having at least one Environment on stack
pub struct EnvStack {
    env: Environment,
    up: Option<Rc<RefCell<EnvStack>>>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            store: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<Rc<RefCell<Object>>> {
        self.store.get(name).map(|var| var.clone())
    }

    fn set(&mut self, name: String, val: Rc<RefCell<Object>>) -> Rc<RefCell<Object>> {
        self.store.insert(name, val.clone());
        val
    }

    fn contains_key(&self, name: &str) -> bool {
        self.store.contains_key(name)
    }
}

impl EnvStack {
    pub fn new() -> Self {
        EnvStack {
            env: Environment::new(),
            up: None,
        }
    }

    pub fn new_enclosed_environment(this: &Rc<RefCell<EnvStack>>) -> Self {
        EnvStack {
            env: Environment::new(),
            up: Some(this.clone()),
        }
    }

    pub fn get(&self, name: &str) -> Option<Rc<RefCell<Object>>> {
        if let val @ Some(_) = self.env.get(name) {
            return val;
        }

        if let Some(up) = &self.up {
            up.borrow().get(name)
        } else {
            None
        }
    }

    pub fn set(&mut self, name: &str, val: Rc<RefCell<Object>>) -> ObjectRc {
        if let Some(obj) = self.find_and_set(name, &val) {
            return obj;
        }

        self.env.set(name.to_string(), val)
    }

    /// Goes up the stack and finds the variable named `name` and set its value.
    /// Returns None if no such variable is found
    fn find_and_set(&mut self, name: &str, val: &Rc<RefCell<Object>>) -> Option<ObjectRc> {
        if self.env.contains_key(name) {
            self.env.set(name.to_string(), val.clone());
            return Some(val.clone());
        }

        self.up
            .as_ref()?
            .try_borrow_mut()
            .expect("unreachable")
            .find_and_set(name, val)
    }
}
