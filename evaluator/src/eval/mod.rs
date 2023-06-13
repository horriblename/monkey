use parser::ast;

use crate::object;

const TRUE: object::Object = object::Object::Bool(true);
const FALSE: object::Object = object::Object::Bool(false);
const NULL: object::Object = object::Object::Null;

// TODO: should be Option<_> (according to the book)
pub fn eval(node: &ast::Node) -> object::Object {
    match node {
        ast::Node::Prog(program) => eval_program(program),
        ast::Node::Stmt(stmt) => eval_statement(stmt),
        ast::Node::Expr(expr) => eval_expression(expr),
    }
}

fn eval_program(program: &ast::Program) -> object::Object {
    let stmts = &program.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(&stmt);
        if let object::Object::ReturnValue(returned) = val {
            return *returned;
        }
        res = Some(val);
    }

    res.expect("TODO: handle statements of length 0")
}

fn eval_block_statement(block: &ast::BlockStatement) -> object::Object {
    let stmts = &block.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(&stmt.borrow());
        if let object::Object::ReturnValue(_) = &val {
            return val;
        }
        res = Some(val);
    }

    res.expect("TODO: handle statements of length 0")
}

fn eval_statement(stmt: &ast::Statement) -> object::Object {
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
            );
            object::Object::ReturnValue(Box::new(value))
        }
        _ => todo!(),
    }
}

fn eval_expression(expr: &ast::Expression) -> object::Object {
    match expr {
        ast::Expression::Int(node) => object::Object::Int(node.value),
        // TODO: return const TRUE/FALSE to reduce object creations
        ast::Expression::Bool(node) => object::Object::Bool(node.value),
        ast::Expression::PrefixExpr(node) => {
            let right = eval_expression(
                &node
                    .operand
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .borrow(),
            );
            eval_prefix_expression(&node.operator.literal, &right)
        }
        ast::Expression::InfixExpr(node) => {
            let left = eval_expression(&node.left_expr.borrow());
            let right = eval_expression(&node.right_expr.as_ref().unwrap().borrow());
            eval_infix_expression(&node.operator.literal, &left, &right)
        }
        ast::Expression::IfExpr(node) => eval_if_expression(node),
        _ => todo!(),
    }
}

fn eval_prefix_expression(operator: &str, right: &object::Object) -> object::Object {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => NULL, // TODO: avoid allocation
    }
}

// NOTE: this implementation is far different from the book. In the book, boolean objects are
// pointers that either point to the TRUE or FALSE const pointer, so they only had to check if
// `right` points to TRUE/FALSE e.g. right == TRUE
fn eval_bang_operator_expression(right: &object::Object) -> object::Object {
    match right {
        object::Object::Bool(true) => FALSE,
        object::Object::Bool(false) => TRUE,
        object::Object::Null => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: &object::Object) -> object::Object {
    match right {
        object::Object::Int(x) => object::Object::Int(-x),
        _ => NULL,
    }
}

fn eval_infix_expression(
    operator: &str,
    left: &object::Object,
    right: &object::Object,
) -> object::Object {
    if let object::Object::Int(left_val) = left {
        if let object::Object::Int(right_val) = right {
            return eval_integer_infix_expression(operator, *left_val, *right_val);
        }
    }

    // NOTE: in the book pointer comparison is used to get slightly better performance. We can't do
    // that here tho
    match operator {
        "==" => return native_bool_to_boolean_object(left == right),
        "!=" => return native_bool_to_boolean_object(left != right),
        _ => NULL,
    }
}

fn eval_integer_infix_expression(operator: &str, left_val: i64, right_val: i64) -> object::Object {
    match operator {
        "+" => object::Object::Int(left_val + right_val),
        "-" => object::Object::Int(left_val - right_val),
        "*" => object::Object::Int(left_val * right_val),
        "/" => object::Object::Int(left_val / right_val),
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => NULL,
    }
}

fn native_bool_to_boolean_object(val: bool) -> object::Object {
    if val {
        TRUE
    } else {
        FALSE
    }
}

fn eval_if_expression(if_expr: &ast::IfExpression) -> object::Object {
    let condition = eval_expression(
        &if_expr
            .condition
            .as_ref()
            .expect("Unchecked Parse Error!")
            .borrow(),
    );

    if is_truthy(&condition) {
        let consequence = if_expr.consequence.borrow();
        eval_block_statement(&consequence)
    } else if let Some(alt) = &if_expr.alternative {
        let block = alt.as_ref().expect("Unchecked Parse Error!").borrow();
        eval_block_statement(&block)
    } else {
        object::Object::Null
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
