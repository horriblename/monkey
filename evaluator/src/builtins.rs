use crate::error::{EResult, EvalError};
use crate::object;

pub fn get_builtin(name: &str) -> Option<&'static BuiltinFuncSignature> {
    match name {
        "len" => Some(&len),
        _ => None,
    }
}

pub type BuiltinFuncSignature = dyn Fn(&Vec<object::ObjectRc>) -> EResult<object::Object>;

fn len(args: &Vec<object::ObjectRc>) -> EResult<object::Object> {
    // TODO: error handling
    if args.len() != 1 {
        return Err(EvalError::WrongArgCount {
            expected: 1,
            got: args.len(),
        });
    }

    match &*args[0].borrow() {
        object::Object::String(s) => Ok(object::Object::Int(s.len() as i64)),
        object::Object::Array(arr) => Ok(object::Object::Int(arr.elements.len() as i64)),
        obj => Err(EvalError::MismatchArgumentType {
            func_name: "len",
            got: obj.type_(),
        }),
    }
}
