use parser::ast::{self, representation::StringRepr};
use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use crate::error::EResult;

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    String,
    Null,
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
#[derive(Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    String(String),
    Null,
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

// #[derive(Debug)]
// pub struct Function<'obj, 'ast: 'obj> {
//     pub parameters: Vec<ast::Identifier>,
//     pub body: &'ast ast::BlockStatement,
//     pub env: &'obj RefCell<Environment<'ast>>,
// }

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::BlockStatement,
    // TODO: remove Option
    pub env: Environment,
}

pub type BuiltinFuncSignature = dyn Fn(&Vec<ObjectRc>) -> EResult<Object>;

pub fn get_builtin(name: &str) -> Option<Builtin> {
    match name {
        "len" => Some(Builtin::new(&builtins::len)),
        _ => None,
    }
}

pub struct Builtin {
    func: &'static BuiltinFuncSignature,
}

impl Builtin {
    pub fn new(func: &'static BuiltinFuncSignature) -> Self {
        Self { func }
    }

    pub fn call(&self, args: &Vec<ObjectRc>) -> EResult<Object> {
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

mod builtins {
    use crate::error::{EResult, EvalError};

    pub fn len(args: &Vec<super::ObjectRc>) -> EResult<super::Object> {
        // TODO: error handling
        if args.len() != 1 {
            return Err(EvalError::WrongArgCount {
                expected: 1,
                got: args.len(),
            });
        }

        match &*args[0].borrow() {
            super::Object::String(s) => Ok(super::Object::Int(s.len() as i64)),
            obj => Err(EvalError::MismatchArgumentType {
                func_name: "len",
                got: obj.type_(),
            }),
        }
    }
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
