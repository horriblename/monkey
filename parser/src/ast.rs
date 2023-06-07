//! # BNF (outdated)
//!
//! Program ::= Statement | Statement Program
//! Statement ::= let Identifier = Expression ";"
//! Expression ::= [undefined]

use lexer::token::Token;
use std::{cell::RefCell, rc::Rc};

type ChildNode<T> = Rc<RefCell<T>>;
// TODO: I could possibly turn ExpectedChild into a Result<Rc<RefCell<T>>, ParserError>
// This give would meaning to the otherwise elusive Option<_>
type ExpectedChild<T> = Option<Rc<RefCell<T>>>;

#[derive(Debug)]
pub enum Node {
    Stmt(Statement),
    Expr(Expression),
}

#[derive(Debug)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expr(ExpressionStatement),
    Block(BlockExpression),
}

#[derive(Debug)]
pub enum Expression {
    Ident(Identifier),
    Int(IntegerLiteral),
    Bool(BooleanLiteral),
    PrefixExpr(PrefixExpression),
    InfixExpr(InfixExpression),
    IfExpr(IfExpression),
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
    pub name: ExpectedChild<Identifier>,

    /// right hand side
    pub value: ExpectedChild<Expression>,
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
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub expr: ExpectedChild<Expression>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expr: ExpectedChild<Expression>,
}

// { statement; ... }
#[derive(Debug)]
pub struct BlockExpression {
    pub statements: Vec<ChildNode<Statement>>,
}

#[derive(Debug)]
pub struct PrefixExpression {
    pub operator: Token,
    pub operand: ExpectedChild<Expression>,
}

#[derive(Debug)]
pub struct InfixExpression {
    pub left_expr: ChildNode<Expression>,
    pub operator: Token,
    pub right_expr: ExpectedChild<Expression>,
}

#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: ExpectedChild<Expression>,
    pub consequence: ChildNode<BlockExpression>,
    /// The first Option indicates whether a follow-up else clause was given.
    ///
    /// The second Option shows if there is an error while parsing the else block.
    pub alternative: Option<ExpectedChild<BlockExpression>>,
}

pub mod representation {

    use super::{
        BlockExpression, BooleanLiteral, Expression, ExpressionStatement, Identifier, IfExpression,
        InfixExpression, IntegerLiteral, LetStatement, Node, PrefixExpression, Program,
        ReturnStatement, Statement,
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
                .join("")
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
                Self::Block(block) => block.string_repr(),
            }
        }
    }

    impl StringRepr for Expression {
        fn string_repr(&self) -> String {
            match self {
                Self::Ident(ident) => ident.string_repr(),
                Self::Int(integer) => integer.string_repr(),
                Self::Bool(b) => b.string_repr(),
                Self::PrefixExpr(expr) => expr.string_repr(),
                Self::InfixExpr(expr) => expr.string_repr(),
                Self::IfExpr(expr) => expr.string_repr(),
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
                self.value.as_ref().unwrap().borrow().string_repr()
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
            self.expr.as_ref().unwrap().borrow().string_repr()
        }
    }

    impl StringRepr for BlockExpression {
        fn string_repr(&self) -> String {
            self.statements
                .iter()
                .map(|stmt| stmt.borrow().string_repr())
                .collect::<Vec<_>>()
                .join("")
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

    impl StringRepr for BooleanLiteral {
        fn string_repr(&self) -> String {
            self.value.to_string()
        }
    }

    impl StringRepr for PrefixExpression {
        fn string_repr(&self) -> String {
            format!(
                "({}{})",
                self.operator.literal,
                self.operand.as_ref().unwrap().borrow().string_repr()
            )
        }
    }

    impl StringRepr for InfixExpression {
        fn string_repr(&self) -> String {
            format!(
                "({} {} {})",
                self.left_expr.borrow().string_repr(),
                self.operator.literal,
                self.right_expr.as_ref().unwrap().borrow().string_repr(),
            )
        }
    }

    impl StringRepr for IfExpression {
        fn string_repr(&self) -> String {
            let mut repr = self.token.literal.to_string();
            repr.push_str(&self.condition.as_ref().unwrap().borrow().string_repr());
            repr.push(' ');
            repr.push_str(&self.consequence.borrow().string_repr());

            if let Some(block) = &self.alternative {
                repr.push_str(" else ");
                repr.push_str(&block.as_ref().unwrap().borrow().string_repr());
            }

            repr
        }
    }
}
