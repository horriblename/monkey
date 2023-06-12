//! I went ham on enums and other rust constructs when writing the parser, but I'm gonna tune it
//! down a bit as evaluators are completely uncharted territory for me. This part will be written as
//! closely to the go code in the book as possible, at least for now.

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
}

//  kind of redundant with ObjectType, might remove one later
#[derive(Debug, PartialEq)]
pub enum ObjectValue {
    Int(i64),
    Bool(bool),
    Null,
}

pub trait Object {
    fn type_(&self) -> ObjectType;
    fn inspect(&self) -> String;
    fn value(&self) -> ObjectValue;
}

#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn type_(&self) -> ObjectType {
        ObjectType::Integer
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn value(&self) -> ObjectValue {
        ObjectValue::Int(self.value)
    }
}

#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn type_(&self) -> ObjectType {
        ObjectType::Boolean
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    fn value(&self) -> ObjectValue {
        ObjectValue::Bool(self.value)
    }
}

#[derive(Debug)]
pub struct Null;

impl Object for Null {
    fn type_(&self) -> ObjectType {
        ObjectType::Null
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }

    fn value(&self) -> ObjectValue {
        ObjectValue::Null
    }
}
