use parser::ast;

use crate::{
    error::{EResult, EvalError, InfixOpError, InfixOpErrorType, UnknownPrefixOperator},
    object,
};

const TRUE: object::Object = object::Object::Bool(true);
const FALSE: object::Object = object::Object::Bool(false);
const NULL: object::Object = object::Object::Null;

// TODO: should be Option<_> (according to the book)
pub fn eval(node: &ast::Node) -> EResult<object::Object> {
    match node {
        ast::Node::Prog(program) => eval_program(program),
        ast::Node::Stmt(stmt) => eval_statement(stmt),
        ast::Node::Expr(expr) => eval_expression(expr),
    }
}

fn eval_program(program: &ast::Program) -> EResult<object::Object> {
    let stmts = &program.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(&stmt)?;
        if let object::Object::ReturnValue(returned) = val {
            return Ok(*returned);
        }
        res = Some(val);
    }

    Ok(res.expect("TODO: handle statements of length 0"))
}

fn eval_block_statement(block: &ast::BlockStatement) -> EResult<object::Object> {
    let stmts = &block.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(&stmt.borrow())?;
        if let object::Object::ReturnValue(_) = &val {
            return Ok(val);
        }
        res = Some(val);
    }

    Ok(res.expect("TODO: handle statements of length 0"))
}

fn eval_statement(stmt: &ast::Statement) -> EResult<object::Object> {
    match stmt {
        ast::Statement::Expr(expr) => {
            eval_expression(&expr.expr.as_ref().expect("Unchecked Parse Error!").borrow())
        }
        ast::Statement::Return(return_stmt) => {
            let value = eval_expression(
                &return_stmt
                    .expr
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow(),
            )?;
            Ok(object::Object::ReturnValue(Box::new(value)))
        }
        _ => todo!(),
    }
}

fn eval_expression(expr: &ast::Expression) -> EResult<object::Object> {
    match expr {
        ast::Expression::Int(node) => Ok(object::Object::Int(node.value)),
        // TODO: return const TRUE/FALSE to reduce object creations
        ast::Expression::Bool(node) => Ok(object::Object::Bool(node.value)),
        ast::Expression::PrefixExpr(node) => {
            let right = eval_expression(
                &node
                    .operand
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow(),
            )?;
            eval_prefix_expression(&node.operator.literal, &right)
        }
        ast::Expression::InfixExpr(node) => {
            let left = eval_expression(&node.left_expr.borrow())?;
            let right = eval_expression(&node.right_expr.as_ref().unwrap().borrow())?;
            eval_infix_expression(&node.operator.literal, &left, &right)
        }
        ast::Expression::IfExpr(node) => eval_if_expression(node),
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: &str, right: &object::Object) -> EResult<object::Object> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(EvalError::UnknownPrefixOp(UnknownPrefixOperator {
            operator: operator.to_string(),
            operand_type: right.type_(),
        })),
    }
}

// NOTE: this implementation is far different from the book. In the book, boolean objects are
// pointers that either point to the TRUE or FALSE const pointer, so they only had to check if
// `right` points to TRUE/FALSE e.g. right == TRUE
fn eval_bang_operator_expression(right: &object::Object) -> EResult<object::Object> {
    match right {
        object::Object::Bool(true) => Ok(FALSE),
        object::Object::Bool(false) => Ok(TRUE),
        object::Object::Null => Ok(TRUE),
        _ => Ok(FALSE),
    }
}

fn eval_minus_prefix_operator_expression(right: &object::Object) -> EResult<object::Object> {
    match right {
        object::Object::Int(x) => Ok(object::Object::Int(-x)),
        _ => Err(EvalError::UnknownPrefixOp(UnknownPrefixOperator {
            operator: "-".to_string(),
            operand_type: right.type_(),
        })),
    }
}

fn eval_infix_expression(
    operator: &str,
    left: &object::Object,
    right: &object::Object,
) -> EResult<object::Object> {
    if let object::Object::Int(left_val) = left {
        if let object::Object::Int(right_val) = right {
            return eval_integer_infix_expression(operator, *left_val, *right_val);
        }
    }

    if left.type_() != right.type_() {
        return Err(EvalError::InfixOpErr(InfixOpError {
            error_type: InfixOpErrorType::TypeMismatch,
            left_type: left.type_(),
            operator: operator.to_string(),
            right_type: right.type_(),
        }));
    }

    // NOTE: in the book pointer comparison is used to get slightly better performance. We can't do
    // that here tho
    match operator {
        "==" => native_bool_to_boolean_object(left == right),
        "!=" => native_bool_to_boolean_object(left != right),
        _ => Err(EvalError::InfixOpErr(InfixOpError {
            error_type: InfixOpErrorType::UnknownOperator,
            left_type: left.type_(),
            operator: operator.to_string(),
            right_type: right.type_(),
        })),
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left_val: i64,
    right_val: i64,
) -> EResult<object::Object> {
    match operator {
        "+" => Ok(object::Object::Int(left_val + right_val)),
        "-" => Ok(object::Object::Int(left_val - right_val)),
        "*" => Ok(object::Object::Int(left_val * right_val)),
        "/" => Ok(object::Object::Int(left_val / right_val)),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => Ok(NULL),
    }
}

fn native_bool_to_boolean_object(val: bool) -> EResult<object::Object> {
    if val {
        Ok(TRUE)
    } else {
        Ok(FALSE)
    }
}

fn eval_if_expression(if_expr: &ast::IfExpression) -> EResult<object::Object> {
    let condition = eval_expression(
        &if_expr
            .condition
            .as_ref()
            .expect("Unchecked Parse Error!")
            .borrow(),
    )?;

    if is_truthy(&condition) {
        let consequence = if_expr.consequence.borrow();
        eval_block_statement(&consequence)
    } else if let Some(alt) = &if_expr.alternative {
        let block = alt.as_ref().expect("Unchecked Parse Error!").borrow();
        eval_block_statement(&block)
    } else {
        Ok(object::Object::Null)
    }
}

/// an expression is considered truthy if: it is not null, and it is not boolean false
fn is_truthy(condition: &object::Object) -> bool {
    // NOTE: pointer comparison was used in the book
    match condition {
        object::Object::Null => false,
        object::Object::Bool(false) => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests;
