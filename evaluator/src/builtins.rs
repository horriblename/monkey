use std::cell::RefCell;
use std::rc::Rc;

use crate::error::{EResult, EvalError};
use crate::object;

pub fn get_builtin(name: &str) -> Option<&'static BuiltinFuncSignature> {
    match name {
        "len" => Some(&len),
        "first" => Some(&first),
        "last" => Some(&last),
        "rest" => Some(&rest),
        "push" => Some(&push),
        "puts" => Some(&puts),
        _ => None,
    }
}

pub type BuiltinFuncSignature = dyn Fn(&Vec<object::ObjectRc>) -> EResult<object::ObjectRc>;

fn len(args: &Vec<object::ObjectRc>) -> EResult<object::ObjectRc> {
    check_arg_count(1, args.len())?;

    match &*args[0].borrow() {
        object::Object::String(s) => Ok(new_rc_refcell(object::Object::Int(s.len() as i64))),
        object::Object::Array(arr) => Ok(new_rc_refcell(object::Object::Int(
            arr.elements.len() as i64
        ))),
        obj => Err(EvalError::MismatchArgumentType {
            func_name: "len",
            got: obj.type_(),
        }),
    }
}

fn first(args: &Vec<object::ObjectRc>) -> EResult<object::ObjectRc> {
    check_arg_count(1, args.len())?;

    let obj = &*args[0].borrow();
    let object::Object::Array(arr) = obj else {
        return Err(EvalError::MismatchArgumentType {
            func_name: "first",
            got: obj.type_(),
        });
    };

    if let Some(first) = arr.elements.first() {
        Ok(first.clone())
    } else {
        Ok(new_rc_refcell(object::Object::Null))
    }
}

fn last(args: &Vec<object::ObjectRc>) -> EResult<object::ObjectRc> {
    check_arg_count(1, args.len())?;

    let obj = &*args[0].borrow();
    let object::Object::Array(arr) = obj else {
        return Err(EvalError::MismatchArgumentType {
            func_name: "last",
            got: obj.type_(),
        });
    };

    if let Some(last) = arr.elements.last() {
        Ok(last.clone())
    } else {
        Ok(new_rc_refcell(object::Object::Null))
    }
}

fn rest(args: &Vec<object::ObjectRc>) -> EResult<object::ObjectRc> {
    check_arg_count(1, args.len())?;

    let obj = &*args[0].borrow();
    let object::Object::Array(arr) = obj else {
        return Err(EvalError::MismatchArgumentType {
            func_name: "rest",
            got: obj.type_(),
        });
    };

    if arr.elements.len() <= 0 {
        return Ok(new_rc_refcell(object::Object::Null));
    }

    let new_array = object::Object::Array(object::Array {
        elements: arr
            .elements
            .iter()
            .skip(1)
            .map(|el| new_rc_refcell(el.borrow().clone()))
            .collect::<Vec<_>>(),
    });
    Ok(new_rc_refcell(new_array))
}

fn push(args: &Vec<object::ObjectRc>) -> EResult<object::ObjectRc> {
    check_arg_count(2, args.len())?;

    let array = args[0].borrow();
    let object::Object::Array(array) = &*args[0].borrow() else {
        return Err(EvalError::MismatchArgumentType { func_name: "push", got: array.type_() });
    };

    let mut new_elements = array
        .elements
        .iter()
        .map(|el| new_rc_refcell(el.borrow().clone()))
        .collect::<Vec<_>>();

    new_elements.push(args[1].clone());
    Ok(new_rc_refcell(object::Object::Array(object::Array {
        elements: new_elements,
    })))
}

fn puts(args: &Vec<object::ObjectRc>) -> EResult<object::ObjectRc> {
    for arg in args {
        println!("{}", arg.borrow().inspect())
    }

    Ok(new_rc_refcell(object::Object::Null))
}

// -----------------
// Helper Functions
// -----------------

fn check_arg_count(expected: usize, got: usize) -> EResult<()> {
    if expected != got {
        Err(EvalError::WrongArgCount { expected, got })
    } else {
        Ok(())
    }
}

#[inline]
fn new_rc_refcell<T>(item: T) -> Rc<RefCell<T>> {
    Rc::new(RefCell::new(item))
}
