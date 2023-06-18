use std::{cell::RefCell, rc::Rc};

use parser::ast;

use crate::{
    error::{
        EResult, EvalError, InfixOpError, InfixOpErrorType, UnknownIdentifier,
        UnknownPrefixOperator,
    },
    object,
};

const TRUE: object::Object = object::Object::Bool(true);
const FALSE: object::Object = object::Object::Bool(false);
const NULL: object::Object = object::Object::Null;

// TODO: should be Option<_> (according to the book)
// FIXME: wrapping all eval return Objects with Rc<RefCell<>> cuz idk how to do things properly
pub fn eval(node: &ast::Node, env: &mut object::Environment) -> EResult<object::ObjectRc> {
    match node {
        ast::Node::Prog(program) => eval_program(program, env),
        ast::Node::Stmt(stmt) => eval_statement(stmt, env),
        ast::Node::Expr(expr) => eval_expression(expr, env),
    }
}

fn eval_program(
    program: &ast::Program,
    env: &mut object::Environment,
) -> EResult<object::ObjectRc> {
    let stmts = &program.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(&stmt, env)?;
        if let object::Object::ReturnValue(returned) = &*val.borrow() {
            return Ok(returned.clone());
        }
        res = Some(val);
    }

    Ok(res.expect("TODO: handle statements of length 0"))
}

fn eval_block_statement(
    block: &ast::BlockStatement,
    env: &mut object::Environment,
) -> EResult<object::ObjectRc> {
    let stmts = &block.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(&stmt.borrow(), env)?;
        if let object::Object::ReturnValue(_) = &*val.borrow() {
            return Ok(val.clone());
        }
        res = Some(val);
    }

    Ok(res.expect("TODO: handle statements of length 0"))
}

fn eval_statement(
    stmt: &ast::Statement,
    env: &mut object::Environment,
) -> EResult<object::ObjectRc> {
    match stmt {
        ast::Statement::Expr(expr) => eval_expression(
            &expr.expr.as_ref().expect("Unchecked Parse Error!").borrow(),
            env,
        ),
        ast::Statement::Return(return_stmt) => {
            let value = eval_expression(
                &return_stmt
                    .expr
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow(),
                env,
            )?;
            Ok(Rc::new(RefCell::new(object::Object::ReturnValue(value))))
        }
        ast::Statement::Let(let_stmt) => {
            let val = eval_expression(
                &let_stmt
                    .value
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow(),
                env,
            )?;
            let var = env.set(
                let_stmt
                    .name
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow()
                    .value
                    .clone(),
                val,
            );
            Ok(var)
        }
        _ => todo!(),
    }
}

fn eval_expression(
    expr: &ast::Expression,
    env: &mut object::Environment,
) -> EResult<object::ObjectRc> {
    match expr {
        ast::Expression::Int(node) => Ok(Rc::new(RefCell::new(object::Object::Int(node.value)))),
        // TODO: return const TRUE/FALSE to reduce object creations
        ast::Expression::Bool(node) => Ok(Rc::new(RefCell::new(object::Object::Bool(node.value)))),
        ast::Expression::PrefixExpr(node) => {
            let right = eval_expression(
                &node
                    .operand
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow(),
                env,
            )?;
            let right_borrow = right.borrow();
            eval_prefix_expression(&node.operator.literal, &right_borrow)
        }
        ast::Expression::InfixExpr(node) => {
            let left_ = eval_expression(&node.left_expr.borrow(), env)?;
            let left = left_.borrow();
            let right_ = eval_expression(&node.right_expr.as_ref().unwrap().borrow(), env)?;
            let right = right_.borrow();
            eval_infix_expression(&node.operator.literal, &left, &right)
        }
        ast::Expression::IfExpr(node) => eval_if_expression(node, env),
        ast::Expression::Ident(ident) => {
            if let Some(var) = env.get(&ident.value) {
                Ok(var)
            } else {
                Err(EvalError::UnknownIdent(UnknownIdentifier {
                    name: ident.value.clone(),
                }))
            }
        }
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: &str, right: &object::Object) -> EResult<object::ObjectRc> {
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
fn eval_bang_operator_expression(right: &object::Object) -> EResult<object::ObjectRc> {
    match right {
        object::Object::Bool(true) => Ok(Rc::new(RefCell::new(FALSE))),
        object::Object::Bool(false) => Ok(Rc::new(RefCell::new(TRUE))),
        object::Object::Null => Ok(Rc::new(RefCell::new(TRUE))),
        _ => Ok(Rc::new(RefCell::new(FALSE))),
    }
}

fn eval_minus_prefix_operator_expression(right: &object::Object) -> EResult<object::ObjectRc> {
    match right {
        object::Object::Int(x) => Ok(Rc::new(RefCell::new(object::Object::Int(-x)))),
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
) -> EResult<object::ObjectRc> {
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
) -> EResult<object::ObjectRc> {
    match operator {
        "+" => Ok(Rc::new(RefCell::new(object::Object::Int(
            left_val + right_val,
        )))),
        "-" => Ok(Rc::new(RefCell::new(object::Object::Int(
            left_val - right_val,
        )))),
        "*" => Ok(Rc::new(RefCell::new(object::Object::Int(
            left_val * right_val,
        )))),
        "/" => Ok(Rc::new(RefCell::new(object::Object::Int(
            left_val / right_val,
        )))),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => Ok(Rc::new(RefCell::new(NULL))),
    }
}

fn native_bool_to_boolean_object(val: bool) -> EResult<object::ObjectRc> {
    if val {
        Ok(Rc::new(RefCell::new(TRUE)))
    } else {
        Ok(Rc::new(RefCell::new(FALSE)))
    }
}

fn eval_if_expression(
    if_expr: &ast::IfExpression,
    env: &mut object::Environment,
) -> EResult<object::ObjectRc> {
    let condition = eval_expression(
        &if_expr
            .condition
            .as_ref()
            .expect("Unchecked Parse Error!")
            .borrow(),
        env,
    )?;

    if is_truthy(&condition.borrow()) {
        let consequence = if_expr.consequence.borrow();
        eval_block_statement(&consequence, env)
    } else if let Some(alt) = &if_expr.alternative {
        let block = alt.as_ref().expect("Unchecked Parse Error!").borrow();
        eval_block_statement(&block, env)
    } else {
        Ok(Rc::new(RefCell::new(object::Object::Null)))
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
