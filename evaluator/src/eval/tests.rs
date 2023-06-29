use std::rc::Rc;

use crate::{error::EResult, object};
use lexer::lexer::Lexer;
use parser::{
    ast::{self, representation::StringRepr},
    parse::Parser,
};

// TODO: extract these into a top-level util crate, this is also used by parser tests
type TResult<T> = Result<T, TError>;
#[derive(Debug)]
struct TError(String);

macro_rules! cast_variant {
    ($target: expr, $pat: path) => {{
        if let $pat(a) = $target {
            Ok(a)
        } else {
            Err(TError(format!(
                "mismatch variant when casting into {} from {:?}",
                stringify!($pat),
                $target,
            )))
        }
    }};
}

macro_rules! check_eq {
    ($left: expr, $right: expr) => {{
        let (lres, rres) = ($left, $right);
        if lres == rres {
            Ok(())
        } else {
            Err(TError(format!(
                "check failed: {} == {}\nleft: {:?}\nright: {:?}",
                stringify!($left),
                stringify!($right),
                lres,
                rres,
            )))
        }
    }};
}

#[test]
fn test_eval_integer_expression() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "5",
            expected: 5,
        },
        Test {
            input: "10",
            expected: 10,
        },
        Test {
            input: "-5",
            expected: -5,
        },
        Test {
            input: "-10",
            expected: -10,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        test_integer_object(&evaluated.borrow(), test.expected).unwrap();
    }
}

fn test_eval(input: &str) -> EResult<object::ObjectRc> {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    let mut env = object::EnvStack::new();

    return super::eval(ast::Node::Prog(program), &mut env);
}

fn test_integer_object(obj: &object::Object, expected: i64) -> TResult<()> {
    check_eq!(&object::Object::Int(expected), obj)
}

fn test_null_object(obj: &object::Object) -> TResult<()> {
    check_eq!(&object::Object::Null, obj)
}

#[test]
fn test_eval_boolean_expression() {
    struct Test {
        input: &'static str,
        expected: bool,
    }

    let tests = vec![
        Test {
            input: "true",
            expected: true,
        },
        Test {
            input: "false",
            expected: false,
        },
        Test {
            input: "true == true",
            expected: true,
        },
        Test {
            input: "false == false",
            expected: true,
        },
        Test {
            input: "true == false",
            expected: false,
        },
        Test {
            input: "true != false",
            expected: true,
        },
        Test {
            input: "false != true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == false",
            expected: false,
        },
        Test {
            input: "(1 > 2) == true",
            expected: false,
        },
        Test {
            input: "(1 > 2) == false",
            expected: true,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        test_boolean_object(&evaluated.borrow(), test.expected).unwrap();
    }
}

fn test_boolean_object(obj: &object::Object, expected: bool) -> TResult<()> {
    check_eq!(&object::Object::Bool(expected), obj)?;

    Ok(())
}

#[test]
fn test_eval_string_expression() {
    struct Test {
        input: &'static str,
        expected: &'static str,
    }

    let tests = vec![Test {
        input: r#""Hello World!""#,
        expected: "Hello World!",
    }];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        test_string_object(&evaluated.borrow(), test.expected).unwrap();
    }
}

fn test_string_object(obj: &object::Object, expected: &str) -> TResult<()> {
    let val = cast_variant!(obj, object::Object::String).unwrap();
    check_eq!(val, expected)?;
    Ok(())
}

#[test]
fn test_bang_operator() {
    struct Test {
        input: &'static str,
        expected: bool,
    }

    let tests = vec![
        Test {
            input: "!true",
            expected: false,
        },
        Test {
            input: "!false",
            expected: true,
        },
        Test {
            input: "!5",
            expected: false,
        },
        Test {
            input: "!!true",
            expected: true,
        },
        Test {
            input: "!!false",
            expected: false,
        },
        Test {
            input: "!!5",
            expected: true,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        test_boolean_object(&evaluated.borrow(), test.expected).unwrap();
    }
}

#[test]
fn test_infix_operator() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "5",
            expected: 5,
        },
        Test {
            input: "10",
            expected: 10,
        },
        Test {
            input: "-5",
            expected: -5,
        },
        Test {
            input: "-10",
            expected: -10,
        },
        Test {
            input: "5 + 5 + 5 + 5 - 10",
            expected: 10,
        },
        Test {
            input: "2 * 2 * 2 * 2 * 2",
            expected: 32,
        },
        Test {
            input: "-50 + 100 + -50",
            expected: 0,
        },
        Test {
            input: "5 * 2 + 10",
            expected: 20,
        },
        Test {
            input: "5 + 2 * 10",
            expected: 25,
        },
        Test {
            input: "20 + 2 * -10",
            expected: 0,
        },
        Test {
            input: "50 / 2 * 2 + 10",
            expected: 60,
        },
        Test {
            input: "2 * (5 + 10)",
            expected: 30,
        },
        Test {
            input: "3 * 3 * 3 + 10",
            expected: 37,
        },
        Test {
            input: "3 * (3 * 3) + 10",
            expected: 37,
        },
        Test {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: 50,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        test_integer_object(&evaluated.borrow(), test.expected).unwrap();
    }
}

#[test]
fn test_if_else_expressions() {
    struct Test {
        input: &'static str,
        expected: object::Object,
    }

    let tests = vec![
        Test {
            input: "if (true) { 10 }",
            expected: object::Object::Int(10),
        },
        Test {
            input: "if (false) { 10 }",
            expected: object::Object::Null,
        },
        Test {
            input: "if (1) { 10 }",
            expected: object::Object::Int(10),
        },
        Test {
            input: "if (1 < 2) { 10 }",
            expected: object::Object::Int(10),
        },
        Test {
            input: "if (1 > 2) { 10 }",
            expected: object::Object::Null,
        },
        Test {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: object::Object::Int(20),
        },
        Test {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: object::Object::Int(10),
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        assert_eq!(&test.expected, &*evaluated.borrow());
    }
}

#[test]
fn test_return_statements() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "return 10;",
            expected: 10,
        },
        Test {
            input: "return 10; 9;",
            expected: 10,
        },
        Test {
            input: "return 2 * 5; 9;",
            expected: 10,
        },
        Test {
            input: "9; return 2 * 5; 9;",
            expected: 10,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        assert_eq!(&object::Object::Int(test.expected), &*evaluated.borrow());
    }
}

#[test]
fn test_error_handling() {
    struct Test {
        input: &'static str,
        expected_msg: &'static str,
    }

    let tests = vec![
        Test {
            input: "5 + true;",
            expected_msg: "type mismatch: Integer + Boolean",
        },
        Test {
            input: "5 + true; 5;",
            expected_msg: "type mismatch: Integer + Boolean",
        },
        Test {
            input: "-true",
            expected_msg: "unknown operator: -Boolean",
        },
        Test {
            input: "true + false;",
            expected_msg: "unknown operator: Boolean + Boolean",
        },
        Test {
            input: "5; true + false; 5",
            expected_msg: "unknown operator: Boolean + Boolean",
        },
        Test {
            input: "if (10 > 1) { true + false; }",
            expected_msg: "unknown operator: Boolean + Boolean",
        },
        Test {
            input: "
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }

                    return 1;
                }
                ",
            expected_msg: "unknown operator: Boolean + Boolean",
        },
        Test {
            input: "foobar",
            expected_msg: "identifier not found: foobar",
        },
        Test {
            input: r#""Hello" - "World""#,
            expected_msg: "unknown operator: String - String",
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input);
        let err = evaluated.as_ref().expect_err(&format!(
            "expected error with message {}, got {:?}",
            test.expected_msg, &evaluated
        ));
        assert_eq!(test.expected_msg, format!("{:?}", err));
    }
}

#[test]
fn test_let_statement() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "let a = 5; a;",
            expected: 5,
        },
        Test {
            input: "let a = 5 * 5; a;",
            expected: 25,
        },
        Test {
            input: "let a = 5; let b = a; b;",
            expected: 5,
        },
        Test {
            input: "let a = 5; let b = a; let c = a + b + 5; c;",
            expected: 15,
        },
    ];

    for test in tests {
        let obj = &test_eval(test.input).unwrap();
        // let var = cast_variant!(obj, object::Object::Variable).unwrap();
        test_integer_object(&obj.borrow(), test.expected).unwrap();
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) {x + 2;};";
    let evaluated = test_eval(input).unwrap();
    let evaluated = evaluated.borrow();
    let func = cast_variant!(&*evaluated, object::Object::Function).unwrap();

    assert_eq!(1, func.parameters.len());
    assert_eq!("x", func.parameters[0].string_repr());

    // pretty sure in the book it's just (x + 2)
    let expected_body = "{ (x + 2) }";

    assert_eq!(expected_body, func.body.string_repr());
}

#[test]
fn test_function_application() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let tests = vec![
        Test {
            input: "let identity = fn(x) { x; }; identity(5);",
            expected: 5,
        },
        Test {
            input: "let identity = fn(x) { return x; }; identity(5);",
            expected: 5,
        },
        Test {
            input: "let double = fn(x) { x * 2; }; double(5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5, 5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
            expected: 20,
        },
        Test {
            input: "fn(x) { x; }(5)",
            expected: 5,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        test_integer_object(&evaluated.borrow(), test.expected).unwrap();
    }
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello" + " " + "World!""#;

    let evaluated = test_eval(input).unwrap();
    let evaluated = &*evaluated.borrow();
    let val = cast_variant!(evaluated, object::Object::String).unwrap();
    assert_eq!(val, "Hello World!");
}

#[test]
fn test_builtin_functions() {
    #[derive(Debug)]
    enum Expect {
        Int(i64),
        Err(&'static str),
    }
    struct Test {
        input: &'static str,
        expected: Expect,
    }

    let tests = vec![
        Test {
            input: r#"len("")"#,
            expected: Expect::Int(0),
        },
        Test {
            input: r#"len("four")"#,
            expected: Expect::Int(4),
        },
        Test {
            input: r#"len("hello world")"#,
            expected: Expect::Int(11),
        },
        Test {
            input: "len(1)",
            expected: Expect::Err("argument to `len` not supported, got Integer"),
        },
        Test {
            input: r#"len("one", "two")"#,
            expected: Expect::Err("wrong number of arguments. got=2, want=1"),
        },
        Test {
            input: "len([1, 2, 3])",
            expected: Expect::Int(3),
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).map(|res| Rc::try_unwrap(res).unwrap().into_inner());

        match (&evaluated, test.expected) {
            (Err(err), Expect::Err(expected)) => assert_eq!(format!("{:?}", err), expected),
            (Ok(object::Object::Int(res)), Expect::Int(expected)) => assert_eq!(res, &expected),
            (res, expected) => panic!("expected {:?} got {:?}", expected, res),
        }
    }
}

#[test]
fn test_array_index_expression() {
    struct Test {
        input: &'static str,
        expected: Option<i64>,
    }
    let tests = vec![
        Test {
            input: "[1, 2, 3][0]",
            expected: Some(1),
        },
        Test {
            input: "[1, 2, 3][1]",
            expected: Some(2),
        },
        Test {
            input: "[1, 2, 3][2]",
            expected: Some(3),
        },
        Test {
            input: "let i = 0; [1][i];",
            expected: Some(1),
        },
        Test {
            input: "[1, 2, 3][1 + 1];",
            expected: Some(3),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[2];",
            expected: Some(3),
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            expected: Some(6),
        },
        Test {
            input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            expected: Some(2),
        },
        Test {
            input: "[1, 2, 3][3]",
            expected: None,
        },
        Test {
            input: "[1, 2, 3][-1]",
            expected: None,
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        let evaluated = &*evaluated.borrow();
        if let Some(expected) = test.expected {
            test_integer_object(evaluated, expected).unwrap();
        } else {
            test_null_object(evaluated).unwrap();
        }
    }
}

#[test]
fn test_hash_literals() {
    let input = r#"
        let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }"#;

    let evaluated = test_eval(input).unwrap();
    let evaluated = &*evaluated.borrow();
    let hash = cast_variant!(evaluated, object::Object::Hash).unwrap();

    let expected = |key: &object::Object| match key {
        object::Object::String(s) if s == "one" => 1,
        object::Object::String(s) if s == "two" => 2,
        object::Object::String(s) if s == "three" => 3,
        object::Object::Int(4) => 4,
        object::Object::Bool(true) => 5,
        object::Object::Bool(false) => 6,
        _ => panic!("unexpected key!"),
    };

    assert_eq!(hash.len(), 6);

    for (key, val) in hash.iter() {
        let expected_val = expected(key);
        let val = &*val.borrow();
        test_integer_object(val, expected_val).unwrap();
    }
}

#[test]
fn test_hash_index_expressions() {
    struct Test {
        input: &'static str,
        expected: Option<i64>,
    }

    let tests = vec![
        Test {
            input: r#"{"foo": 5}["foo"]"#,
            expected: Some(5),
        },
        Test {
            input: r#"{"foo": 5}["bar"]"#,
            expected: None,
        },
        Test {
            input: r#"let key = "foo"; {"foo": 5}[key]"#,
            expected: Some(5),
        },
        Test {
            input: r#"{}["foo"]"#,
            expected: None,
        },
        Test {
            input: r#"{5: 5}[5]"#,
            expected: Some(5),
        },
        Test {
            input: r#"{true: 5}[true]"#,
            expected: Some(5),
        },
        Test {
            input: r#"{false: 5}[false]"#,
            expected: Some(5),
        },
    ];

    for test in tests {
        let evaluated = test_eval(test.input).unwrap();
        let evaluated = &*evaluated.borrow();
        if let Some(n) = test.expected {
            test_integer_object(evaluated, n).unwrap();
        } else {
            test_null_object(evaluated).unwrap();
        }
    }
}
