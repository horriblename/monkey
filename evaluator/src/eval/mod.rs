#![allow(dead_code)]

use std::ops::Deref;

use parser::ast;

use crate::object::{self, Object};

const TRUE: object::Boolean = object::Boolean { value: true };
const FALSE: object::Boolean = object::Boolean { value: false };
const NULL: object::Null = object::Null;

// TODO: should be Option<_> (according to the book)
pub fn eval(node: &ast::Node) -> Box<dyn Object> {
    match node {
        ast::Node::Prog(program) => eval_statements(program.statements.iter()),
        ast::Node::Stmt(stmt) => eval_statement(stmt),
        ast::Node::Expr(expr) => eval_expression(expr),
    }
}

fn eval_statements<I, R>(stmts: I) -> Box<dyn Object>
where
    I: Iterator<Item = R>,
    R: Deref<Target = ast::Statement>,
{
    let mut res = None;

    for stmt in stmts {
        res = Some(eval_statement(&stmt));
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
            eval_infix_expression(&node.operator.literal, left.as_ref(), right.as_ref())
        }
        ast::Expression::IfExpr(node) => eval_if_expression(node),
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

fn eval_infix_expression(
    operator: &str,
    left: &dyn object::Object,
    right: &dyn object::Object,
) -> Box<dyn Object> {
    if let object::ObjectValue::Int(left_val) = left.value() {
        if let object::ObjectValue::Int(right_val) = right.value() {
            return eval_integer_infix_expression(operator, left_val, right_val);
        }
    }

    // NOTE: in the book pointer comparison is used to get slightly better performance. We can't do
    // that here tho
    match operator {
        "==" => return native_bool_to_boolean_object(left.value() == right.value()),
        "!=" => return native_bool_to_boolean_object(left.value() != right.value()),
        _ => Box::new(NULL),
    }
}

fn eval_integer_infix_expression(operator: &str, left_val: i64, right_val: i64) -> Box<dyn Object> {
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
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => Box::new(NULL),
    }
}

fn native_bool_to_boolean_object(val: bool) -> Box<object::Boolean> {
    Box::new(if val { TRUE } else { FALSE })
}

fn eval_if_expression(if_expr: &ast::IfExpression) -> Box<dyn Object> {
    let condition = eval_expression(
        &if_expr
            .condition
            .as_ref()
            .expect("Unchecked Parse Error!")
            .borrow(),
    );

    if is_truthy(condition.as_ref()) {
        let consequence = if_expr.consequence.borrow();
        let statements = consequence.statements.iter().map(|x| x.borrow());
        eval_statements(statements)
    } else if let Some(alt) = &if_expr.alternative {
        let block = alt.as_ref().expect("Unchecked Parse Error!").borrow();
        let statements = block.statements.iter().map(|x| x.borrow());
        eval_statements(statements)
    } else {
        Box::new(object::Null)
    }
}

/// an expression is considered truthy if: it is not null, and it is not boolean false
fn is_truthy(condition: &dyn Object) -> bool {
    // NOTE: pointer comparison was used in the book
    match condition.value() {
        object::ObjectValue::Null => false,
        object::ObjectValue::Bool(cond) => cond,
        _ => true,
    }
}

#[cfg(test)]
mod tests;
