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
        ast::Expression::PrefixExpr(node) => {
            let right = eval_expression(
                &node
                    .operand
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow(),
            );
            eval_prefix_expression(&node.operator.literal, right.as_ref())
        }
        ast::Expression::InfixExpr(node) => {
            let left = eval_expression(&node.left_expr.borrow());
            let right = eval_expression(&node.right_expr.as_ref().unwrap().borrow());
            eval_integer_infix_expression(&node.operator.literal, left.as_ref(), right.as_ref())
        }
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: &str, right: &dyn object::Object) -> Box<dyn Object> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Box::new(NULL), // TODO: avoid allocation
    }
}

// NOTE: this implementation is far different from the book. In the book, boolean objects are
// pointers that either point to the TRUE or FALSE const pointer, so they only had to check if
// `right` points to TRUE/FALSE e.g. right == TRUE
fn eval_bang_operator_expression(right: &dyn object::Object) -> Box<dyn Object> {
    match right.value() {
        object::ObjectValue::Bool(true) => Box::new(FALSE),
        object::ObjectValue::Bool(false) => Box::new(TRUE),
        object::ObjectValue::Null => Box::new(TRUE),
        _ => Box::new(FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: &dyn object::Object) -> Box<dyn Object> {
    match right.value() {
        object::ObjectValue::Int(x) => Box::new(object::Integer { value: -x }),
        _ => Box::new(NULL),
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: &dyn object::Object,
    right: &dyn object::Object,
) -> Box<dyn Object> {
    let left_val = if let object::ObjectValue::Int(x) = left.value() {
        x
    } else {
        todo!("Non int infix operand")
    };
    let right_val = if let object::ObjectValue::Int(x) = right.value() {
        x
    } else {
        todo!("Non int infix operand")
    };

    match operator {
        "+" => Box::new(object::Integer {
            value: left_val + right_val,
        }),
        "-" => Box::new(object::Integer {
            value: left_val - right_val,
        }),
        "*" => Box::new(object::Integer {
            value: left_val * right_val,
        }),
        "/" => Box::new(object::Integer {
            value: left_val / right_val,
        }),
        _ => Box::new(NULL),
    }
}

#[cfg(test)]
mod tests;
