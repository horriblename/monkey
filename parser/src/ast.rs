//! # BNF
//!
//! Program ::= Statement | Statement Program
//! Statement ::= let Identifier = Expression ";"
//! Expression ::= [undefined]
#![allow(dead_code)]

use lexer::token::Token;
use std::{cell::RefCell, rc::Rc};

type ChildNode<T> = Rc<RefCell<T>>;

pub enum Node {
    // TODO maybe not put Program as a variant, since it's only used "once"
    Stmt(Statement),
    Expr(Expression),
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

pub enum Expression {
    Ident(Identifier),
    TempDummy,
}

pub struct Program {
    pub statements: Vec<Statement>,
}

pub struct LetStatement {
    pub token: Token,

    /// left haand side
    // NOTE: this is Option<_> to make it possible to parse erroneous input without panicing
    // might make it something else in the future
    pub name: Option<ChildNode<Identifier>>,

    /// right hand side
    pub value: ChildNode<Expression>,
}

pub struct Identifier {
    pub token: Token,
    pub value: String, // isn't this the same as `Token.literal`??
}

pub struct ReturnStatement {
    pub token: Token,
    pub expr: ChildNode<Expression>,
}

pub mod representation {
    use std::fmt::Write;

    use super::{
        Expression, ExpressionStatement, Identifier, LetStatement, Node, Program, ReturnStatement,
        Statement,
    };

    pub trait StringRepr {
        fn string_repr(&self) -> String;
    }

    impl StringRepr for Program {
        fn string_repr(&self) -> String {
            let mut buf = String::new();
            for stmt in &self.statements {
                write!(&mut buf, "{}\n", stmt.string_repr()).expect("todo");
            }
            buf
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
            self.expr.string_repr() + ";"
        }
    }

    impl StringRepr for Identifier {
        fn string_repr(&self) -> String {
            self.value.clone()
        }
    }
}
