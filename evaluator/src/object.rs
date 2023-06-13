
#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Boolean,
    Null,
    ReturnValue,
}

#[derive(Debug)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Null,
    ReturnValue(Box<Object>),
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Int(val) => format!("{}", val),
            Self::Bool(val) => format!("{}", val),
            Self::Null => "null".to_string(),
            Self::ReturnValue(val) => val.inspect(),
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
