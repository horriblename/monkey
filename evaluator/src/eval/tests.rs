use crate::{error::EResult, object};
use lexer::lexer::Lexer;
use parser::{ast, parse::Parser};

// TODO: extract these into a top-level util crate, this is also used by parser tests
type TResult<T> = Result<T, TError>;
#[derive(Debug)]
struct TError(String);

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
    let mut env = object::Environment::new();

    return super::eval(&ast::Node::Prog(program), &mut env);
}

fn test_integer_object(obj: &object::Object, expected: i64) -> TResult<()> {
    check_eq!(&object::Object::Int(expected), obj)?;

    Ok(())
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
