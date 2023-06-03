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
    pub name: ChildNode<Identifier>,

    /// right hand side
    pub value: ChildNode<Expression>,
}

pub struct Identifier {
    pub token: Token,
    pub value: String, // isn't this the same as `Token.literal`??
}

// pub fn get_node_token(node: Node) -> Token {
//     use Expression::*;
//     use Node::*;
//     use Statement::*;
//
//     match node {
//         Stmt(Let(n)) => n.token,
//         Expr(Ident(n)) => n.token,
//     }
// }
