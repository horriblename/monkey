#![allow(dead_code)]

use parser::ast;

use crate::object::{self, Object};

const TRUE: object::Boolean = object::Boolean { value: true };
const FALSE: object::Boolean = object::Boolean { value: false };
const NULL: object::Null = object::Null;

// TODO: should be Option<_> (according to the book)
pub fn eval(node: &ast::Node) -> Box<dyn Object> {
    match node {
        ast::Node::Prog(program) => eval_statements(&program.statements),
        ast::Node::Stmt(stmt) => eval_statement(stmt),
        ast::Node::Expr(expr) => eval_expression(expr),
    }
}

fn eval_statements(stmts: &[ast::Statement]) -> Box<dyn Object> {
    let mut res = None;

    for stmt in stmts {
        res = Some(eval_statement(stmt));
    }

    res.expect("TODO: handle statements of length 0")
}

fn eval_statement(stmt: &ast::Statement) -> Box<dyn Object> {
    match stmt {
        ast::Statement::Expr(expr) => {
            eval_expression(&expr.expr.as_ref().expect("Unchecked Parse Error!").borrow())
        }
        _ => todo!(),
    }
}

fn eval_expression(expr: &ast::Expression) -> Box<dyn Object> {
    match expr {
        ast::Expression::Int(node) => Box::new(object::Integer { value: node.value }),
        // TODO: return const TRUE/FALSE to reduce object creations
        ast::Expression::Bool(node) => Box::new(object::Boolean { value: node.value }),
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests;
