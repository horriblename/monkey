use std::fmt::{Debug, Display};

use crate::object;

pub type EResult<T> = Result<T, EvalError>;

pub enum EvalError {
    UnknownPrefixOp(UnknownPrefixOperator),
    InfixOpErr(InfixOpError),
    UnknownIdent(UnknownIdentifier),
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownPrefixOp(variant) => Display::fmt(variant, f),
            Self::InfixOpErr(variant) => Display::fmt(variant, f),
            Self::UnknownIdent(variant) => Display::fmt(variant, f),
        }
    }
}

impl Debug for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

pub struct UnknownPrefixOperator {
    pub operator: String,
    pub operand_type: object::ObjectType,
}

impl Debug for UnknownPrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "unknown operator: {}{:?}",
            self.operator, self.operand_type
        )
    }
}

impl Display for UnknownPrefixOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

pub enum InfixOpErrorType {
    UnknownOperator,
    TypeMismatch,
}

pub struct InfixOpError {
    pub error_type: InfixOpErrorType,
    pub left_type: object::ObjectType,
    pub operator: String,
    pub right_type: object::ObjectType,
}

impl Debug for InfixOpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.error_type {
            InfixOpErrorType::TypeMismatch => write!(
                f,
                "type mismatch: {:?} {} {:?}",
                self.left_type, self.operator, self.right_type
            ),
            InfixOpErrorType::UnknownOperator => write!(
                f,
                "unknown operator: {:?} {} {:?}",
                self.left_type, self.operator, self.right_type
            ),
        }
    }
}

impl Display for InfixOpError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

pub struct UnknownIdentifier {
    pub name: String,
}

impl Debug for UnknownIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "identifier not found: {}", self.name)
    }
}

impl Display for UnknownIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}