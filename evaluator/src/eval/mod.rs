use std::{cell::RefCell, rc::Rc};

use parser::ast;

use crate::{
    error::{
        self, EResult, EvalError, InfixOpError, InfixOpErrorType, UnknownIdentifier,
        UnknownPrefixOperator,
    },
    object,
};

const TRUE: object::Object = object::Object::Bool(true);
const FALSE: object::Object = object::Object::Bool(false);
const NULL: object::Object = object::Object::Null;

// TODO: should be Option<_> (according to the book)
// FIXME: wrapping all eval return Objects with Rc<RefCell<>> cuz idk how to do things properly
pub fn eval(node: ast::Node, env: Rc<RefCell<object::EnvStack>>) -> EResult<object::ObjectRc> {
    match node {
        ast::Node::Prog(program) => eval_program(program, env),
        ast::Node::Stmt(stmt) => eval_statement(stmt, env),
        ast::Node::Expr(expr) => eval_expression(expr, env),
    }
}

fn eval_program(
    program: ast::Program,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    let stmts = program.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(stmt, env.clone())?;
        if let object::Object::ReturnValue(returned) = &*val.borrow() {
            return Ok(returned.clone());
        }
        res = Some(val);
    }

    Ok(res.expect("TODO: handle statements of length 0"))
}

fn eval_block_statement(
    block: ast::BlockStatement,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    let stmts = block.statements;
    let mut res = None;

    for stmt in stmts {
        let val = eval_statement(*stmt, env.clone())?;
        if let object::Object::ReturnValue(_) = &*val.borrow() {
            return Ok(val.clone());
        }
        res = Some(val);
    }

    Ok(res.expect("TODO: handle statements of length 0"))
}

fn eval_statement(
    stmt: ast::Statement,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    match stmt {
        ast::Statement::Expr(expr) => {
            eval_expression(*expr.expr.expect("Unchecked Parse Error!"), env)
        }
        ast::Statement::Return(return_stmt) => {
            let value = eval_expression(*return_stmt.expr.expect("Unchecked Parse Error!"), env)?;
            Ok(Rc::new(RefCell::new(object::Object::ReturnValue(value))))
        }
        ast::Statement::Let(let_stmt) => {
            let val = eval_expression(
                *let_stmt.value.expect("Unchecked Parse Error!"),
                env.clone(),
            )?;
            let var = env.borrow_mut().set(
                &let_stmt
                    .name
                    .as_ref()
                    .expect("Unchecked Parse Error!")
                    .value,
                val,
            );
            Ok(var)
        }
        ast::Statement::Block(_) => {
            unreachable!("Parser has not supported arbitrary block statements")
        }
    }
}

fn eval_expression(
    expr: ast::Expression,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    match expr {
        ast::Expression::Int(node) => Ok(Rc::new(RefCell::new(object::Object::Int(node.value)))),
        // TODO: return const TRUE/FALSE to reduce object creations
        ast::Expression::Bool(node) => Ok(Rc::new(RefCell::new(object::Object::Bool(node.value)))),
        ast::Expression::String(node) => {
            Ok(Rc::new(RefCell::new(object::Object::String(node.value))))
        }
        ast::Expression::Array(node) => {
            let expressions = node
                .elements
                .into_iter()
                .map(|expr| expr.expect("Unchecked Parse Error!"))
                .collect::<Vec<_>>();
            let elements = eval_expressions(expressions, env)?;
            Ok(Rc::new(RefCell::new(object::Object::Array(
                object::Array { elements },
            ))))
        }
        ast::Expression::Hash(node) => {
            let mut hash_table = object::Hash::new();
            for (k, v) in node.pairs {
                let k = k.expect("Unchecked Parse Error!");
                // I hate this
                let key = Rc::try_unwrap(eval_expression(k, env.clone())?)
                    .map_or_else(|err| err.borrow().clone(), |ok| ok.into_inner());
                let val = eval_expression(v.expect("Unchecked Parse Error!"), env.clone())?;

                hash_table.insert(key, val);
            }

            Ok(Rc::new(RefCell::new(object::Object::Hash(hash_table))))
        }
        ast::Expression::PrefixExpr(node) => {
            let right_ = node.operand.expect("Unchecked Parse Error!");
            let right = eval_expression(*right_, env)?;
            let right_borrow = right.borrow();
            eval_prefix_expression(&node.operator.literal, &right_borrow)
        }
        ast::Expression::InfixExpr(node) => {
            let left_ = eval_expression(*node.left_expr, env.clone())?;
            let left = left_.borrow();
            let right_ = eval_expression(*node.right_expr.expect("Unchecked Parse Error!"), env)?;
            let right = right_.borrow();
            eval_infix_expression(&node.operator.literal, &left, &right)
        }
        ast::Expression::IfExpr(node) => eval_if_expression(node, env),
        ast::Expression::Ident(ident) => eval_identifier(ident, env),
        ast::Expression::Fn(func) => {
            let parameters = func
                .parameters
                .as_ref()
                .expect("Unchecked Parse Error!")
                .clone();
            let body = func.body;
            Ok(Rc::new(RefCell::new(object::Object::Function(
                object::Function {
                    parameters,
                    body,
                    env: env.clone(),
                },
            ))))
        }
        ast::Expression::Call(node) => eval_call_expression(node, env),
        ast::Expression::Index(node) => eval_index_expression(node, env),
    }
}

// fn eval_

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

    match (left, operator, right) {
        (object::Object::String(l), "+", object::Object::String(r)) => Ok(Rc::new(RefCell::new(
            object::Object::String(format!("{}{}", l, r)),
        ))),
        // NOTE: in the book pointer comparison is used to get slightly better performance. We can't do
        // that here tho
        (l, "==", r) => native_bool_to_boolean_object(l == r),
        (l, "!=", r) => native_bool_to_boolean_object(l != r),
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
    if_expr: ast::IfExpression,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    let condition = eval_expression(
        *if_expr.condition.expect("Unchecked Parse Error!"),
        env.clone(),
    )?;

    if is_truthy(&condition.borrow()) {
        let consequence = if_expr.consequence;
        eval_block_statement(consequence, env)
    } else if let Some(alt) = if_expr.alternative {
        let block = alt.expect("Unchecked Parse Error!");
        eval_block_statement(block, env)
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

fn eval_identifier(
    ident: ast::Identifier,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    if let Some(func) = object::Builtin::from_func_name(&ident.value) {
        return Ok(Rc::new(RefCell::new(object::Object::BuiltinFunc(func))));
    }

    if let Some(var) = env.borrow().get(&ident.value) {
        Ok(var)
    } else {
        Err(EvalError::UnknownIdent(UnknownIdentifier {
            name: ident.value.clone(),
        }))
    }
}

fn eval_call_expression(
    node: ast::CallExpression,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    let func = eval_expression(*node.function, env.clone())?;
    let args = eval_expressions(
        node.arguments
            .into_iter()
            .map(|arg| *arg.expect("Unchecked Parse Error!"))
            .collect::<Vec<_>>(),
        env.clone(),
    )?;
    apply_function(func, args, env)
}

fn eval_index_expression(
    node: ast::IndexExpression,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    let left = eval_expression(*node.left, env.clone())?;
    let left = &*left.borrow();
    match left {
        object::Object::Array(left) => {
            let max = left.elements.len() as i64;
            let index_node = *node.index.expect("Unchecked Parse Error!");
            let index = eval_expression(index_node, env)?;
            let index = &*index.borrow();

            if let object::Object::Int(index) = index {
                let index = *index;
                if index < 0 || index >= max {
                    Ok(Rc::new(RefCell::new(NULL)))
                } else {
                    Ok(left.elements[index as usize].clone())
                }
            } else {
                Ok(Rc::new(RefCell::new(NULL)))
            }
        }
        object::Object::Hash(left) => {
            let index_node = *node.index.expect("Unchecked Parse Error!");
            let index = eval_expression(index_node, env)?;
            let index = &*index.borrow();

            Ok(left
                .get(index)
                .unwrap_or_else(|| Rc::new(RefCell::new(NULL))))
        }
        _ => Err(EvalError::IndexOpWrongType {
            left_type: left.type_(),
        }),
    }
}

fn eval_expressions(
    exprs: Vec<ast::Expression>,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<Vec<object::ObjectRc>> {
    let mut res = Vec::with_capacity(exprs.len());

    for expr in exprs {
        let val = eval_expression(expr, env.clone())?;
        res.push(val);
    }

    Ok(res)
}

// NOTE: in the book, environements kinda like linked list, and the environment stored in
// object::Function is used in new_enclosed_environment() instead.
// we'll probably have to do variable capturing some other way, idk
fn apply_function(
    func: object::ObjectRc,
    args: Vec<object::ObjectRc>,
    env: Rc<RefCell<object::EnvStack>>,
) -> EResult<object::ObjectRc> {
    match &*func.borrow() {
        object::Object::BuiltinFunc(func) => Ok(func.call(&args)?),
        object::Object::Function(func) => {
            extend_function_env(func, args, env.clone());
            let evaluated = eval_block_statement(func.body.clone(), env)?;
            Ok(unwrap_return_value(evaluated))
        }
        x => Err(error::EvalError::NotAFunction(error::NotAFunction {
            obj: x.type_(),
        })),
    }
}

fn extend_function_env(
    func: &object::Function,
    args: Vec<object::ObjectRc>,
    env: Rc<RefCell<object::EnvStack>>,
) {
    object::EnvStack::new_enclosed_environment(&env);

    for (arg_name, arg) in func.parameters.iter().zip(args) {
        env.borrow_mut().set(&arg_name.value, arg);
    }
}

fn unwrap_return_value(obj: object::ObjectRc) -> object::ObjectRc {
    if let object::Object::ReturnValue(returned) = &*obj.borrow() {
        return returned.clone();
    }

    obj
}

#[cfg(test)]
mod tests;
