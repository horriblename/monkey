use crate::object::{Object, ObjectType, ObjectValue};
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
        let evaluated = test_eval(test.input);
        test_integer_object(evaluated.as_ref(), test.expected).unwrap();
    }
}

fn test_eval(input: &str) -> Box<dyn Object> {
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    return super::eval(&ast::Node::Prog(program));
}

fn test_integer_object(obj: &dyn Object, expected: i64) -> TResult<()> {
    check_eq!(ObjectType::Integer, obj.type_())?;
    check_eq!(ObjectValue::Int(expected), obj.value())?;

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
    ];

    for test in tests {
        let evaluated = test_eval(test.input);
        test_boolean_object(evaluated.as_ref(), test.expected).unwrap();
    }
}

fn test_boolean_object(obj: &dyn Object, expected: bool) -> TResult<()> {
    check_eq!(ObjectType::Boolean, obj.type_())?;
    check_eq!(ObjectValue::Bool(expected), obj.value())?;

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
        let evaluated = test_eval(test.input);
        test_boolean_object(evaluated.as_ref(), test.expected).unwrap();
    }
}
