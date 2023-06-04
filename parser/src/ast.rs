//! # BNF
//!
//! Program ::= Statement | Statement Program
//! Statement ::= let Identifier = Expression ";"
//! Expression ::= [undefined]
#![allow(dead_code)]

use lexer::token::Token;
use std::{cell::RefCell, rc::Rc};

type ChildNode<T> = Rc<RefCell<T>>;

#[derive(Debug)]
pub enum Node {
    // TODO maybe not put Program as a variant, since it's only used "once"
    Stmt(Statement),
    Expr(Expression),
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
}

#[derive(Debug)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    PrefixExpr(PrefixExpression),
    TempDummy,
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,

    /// left haand side
    // NOTE: this is Option<_> to make it possible to parse erroneous input without panicing
    // might make it something else in the future
    pub name: Option<ChildNode<Identifier>>,

    /// right hand side
    pub value: ChildNode<Expression>,
}

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String, // isn't this the same as `Token.literal`??
}

#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub expr: ChildNode<Expression>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expr: ChildNode<Expression>,
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub operator: Token,
    pub operand: ChildNode<Expression>,
}

pub mod representation {

    use super::{
        Expression, ExpressionStatement, Identifier, IntegerLiteral, LetStatement, Node,
        PrefixExpression, Program, ReturnStatement, Statement,
    };

    pub trait StringRepr {
        fn string_repr(&self) -> String;
    }

    impl StringRepr for Program {
        fn string_repr(&self) -> String {
            self.statements
                .iter()
                .map(|stmt| stmt.string_repr())
                .collect::<Vec<_>>()
                .join("\n")
        }
    }

    impl StringRepr for Node {
        fn string_repr(&self) -> String {
            match self {
                Node::Stmt(stmt) => stmt.string_repr(),
                Node::Expr(expr) => expr.string_repr(),
            }
        }
    }

    impl StringRepr for Statement {
        fn string_repr(&self) -> String {
            match self {
                Self::Let(stmt) => stmt.string_repr(),
                Self::Return(stmt) => stmt.string_repr(),
                Self::Expr(stmt) => stmt.string_repr(),
            }
        }
    }

    impl StringRepr for Expression {
        fn string_repr(&self) -> String {
            match self {
                Self::Ident(ident) => ident.string_repr(),
                Self::Int(integer) => integer.string_repr(),
                Self::PrefixExpr(expr) => expr.string_repr(),
                Self::TempDummy => "<TempDummy>".to_string(),
            }
        }
    }

    impl StringRepr for LetStatement {
        fn string_repr(&self) -> String {
            format!(
                "{} {} = {};",
                self.token.literal,
                self.name
                    .as_ref()
                    .and_then(|name| Some(name.as_ref().borrow().string_repr()))
                    .unwrap_or_else(|| "<None>".to_string()),
                self.value.as_ref().borrow().string_repr()
            )
        }
    }

    impl StringRepr for ReturnStatement {
        fn string_repr(&self) -> String {
            todo!()
        }
    }

    impl StringRepr for ExpressionStatement {
        fn string_repr(&self) -> String {
            self.expr.as_ref().borrow().string_repr() + ";"
        }
    }

    impl StringRepr for Identifier {
        fn string_repr(&self) -> String {
            self.value.clone()
        }
    }

    impl StringRepr for IntegerLiteral {
        fn string_repr(&self) -> String {
            self.value.to_string()
        }
    }

    impl StringRepr for PrefixExpression {
        fn string_repr(&self) -> String {
            format!(
                "{}{}",
                self.operator.literal,
                self.operand.borrow().string_repr()
            )
        }
    }
}
