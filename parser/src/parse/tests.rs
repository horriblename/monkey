use super::Parser;
use crate::ast::{
    representation::StringRepr, Expression, Identifier, LetStatement, Program, Statement,
};
use lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};

#[derive(Debug)]
enum LiteralValue {
    Int(i64),
    Str(String),
    Bool(bool),
}

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

fn print_error<T>(err: TError) -> TResult<T> {
    eprintln!("{:?}", err);
    Err(err)
}

#[test]
fn test_let_statements() {
    struct Test {
        input: &'static str,
        expected_identifier: &'static str,
        expected_value: &'static str,
    }
    let tests = vec![
        Test {
            input: "let x = 5;",
            expected_identifier: "x",
            expected_value: "5",
        },
        Test {
            input: "let y = true;",
            expected_identifier: "y",
            expected_value: "true",
        },
        Test {
            input: "let foobar = y;",
            expected_identifier: "foobar",
            expected_value: "y",
        },
    ];

    for test in tests {
        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        assert_eq!(1, program.statements.len());

        let let_stmt = &program.statements[0];
        let let_stmt = cast_variant!(&let_stmt, Statement::Let).unwrap();
        assert_eq!(
            test.expected_identifier,
            let_stmt.name.as_ref().unwrap().value
        );
        assert!(test_let_statement(
            let_stmt,
            test.expected_identifier,
            test.expected_value
        ));
    }
}

#[test]
fn test_return_statements() {
    struct Test {
        input: &'static str,
        expected_value: LiteralValue,
    }

    let tests = vec![
        Test {
            input: "return 50;",
            expected_value: LiteralValue::Int(50),
        },
        Test {
            input: "return y;",
            expected_value: LiteralValue::Str("y".to_string()),
        },
    ];

    for test in tests {
        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parse_errors(&parser);

        assert_eq!(1, program.statements.len());

        let return_stmt = &program.statements[0];
        let return_stmt = cast_variant!(return_stmt, Statement::Return).unwrap();

        assert_eq!(TokenType::Return, return_stmt.token.type_);
        assert_eq!("return", return_stmt.token.literal);

        let return_expr = &*return_stmt.expr.as_ref().unwrap();
        test_literal_expression(return_expr, &test.expected_value).unwrap();
    }
}

fn test_let_statement(stmt: &LetStatement, name: &str, value: &str) -> bool {
    assert_eq!("let", stmt.token.literal);

    let name_node = stmt.name.as_ref().unwrap();
    assert_eq!(name, name_node.token.literal);

    let value_node = stmt.value.as_ref().unwrap();
    assert_eq!(value, value_node.string_repr());
    true
}

fn check_parse_errors(parser: &Parser) {
    // TODO
    for i in &parser.errors {
        println!("Parser error: {:?}", i);
    }

    assert!(parser.errors.is_empty());
}

#[test]
fn test_string_repr() {
    use crate::ast::representation::StringRepr;

    let program = Program {
        statements: vec![Statement::Let(LetStatement {
            token: Token {
                type_: TokenType::Let,
                literal: "let".to_string(),
            },
            name: Some(Box::new(Identifier {
                token: Token {
                    type_: TokenType::Ident,
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            })),
            value: Some(Box::new(Expression::Ident(Identifier {
                token: Token {
                    type_: TokenType::Ident,
                    literal: "anotherVar".to_string(),
                },
                value: "anotherVar".to_string(),
            }))),
        })],
    };

    assert_eq!("let myVar = anotherVar;", program.string_repr());
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parse_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program has not enough statements, got = {}",
            program.statements.len()
        );
    }

    if let Statement::Expr(expr) = &program.statements[0] {
        let expr = expr.expr.as_ref().unwrap();
        if let Expression::Ident(ident) = &**expr {
            assert_eq!("foobar", ident.value);
            assert_eq!("foobar", ident.token.literal);
        } else {
            panic!("expected Expression::Ident, got {:?}", expr);
        }
    } else {
        panic!("expected ExpressionStatement, got something else");
    }
}

#[test]
fn test_int_expression() {
    let input = "5;".to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parse_errors(&parser);

    if program.statements.len() != 1 {
        panic!(
            "program has not enough statements, got = {}",
            program.statements.len()
        );
    }

    if let Statement::Expr(expr) = &program.statements[0] {
        let expr = expr.expr.as_ref().expect("expected Expression, got None");
        if let Expression::Int(integer) = &**expr {
            assert_eq!(5, integer.value);
            assert_eq!("5", integer.token.literal);
        } else {
            panic!("expected Expression::Ident, got {:?}", expr);
        }
    } else {
        panic!("expected ExpressionStatement, got something else");
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    struct Test {
        input: &'static str,
        operator: &'static str,
        integer_value: i64,
    }

    let tests = vec![
        Test {
            input: "!5;",
            operator: "!",
            integer_value: 5,
        },
        Test {
            input: "-15;",
            operator: "-",
            integer_value: 15,
        },
    ];

    for t in tests {
        let lexer = Lexer::new(t.input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(&parser);

        assert_eq!(program.statements.len(), 1);

        let stmt = &program.statements[0];
        let expr = &*cast_variant!(stmt, Statement::Expr)
            .unwrap()
            .expr
            .as_ref()
            .unwrap();

        let prefix_expr = cast_variant!(&**expr, Expression::PrefixExpr).unwrap();

        assert_eq!(t.operator, prefix_expr.operator.literal);

        let right_expr = &*prefix_expr.operand.as_ref().unwrap();
        let rvalue = cast_variant!(&**right_expr, Expression::Int).unwrap();

        assert_eq!(t.integer_value, rvalue.value);
    }
}

#[test]
fn test_parsing_infix_expressions() {
    struct Test {
        input: &'static str,
        left_value: LiteralValue,
        operator: &'static str,
        right_value: LiteralValue,
    }

    let tests = vec![
        Test {
            input: "5 + 5;",
            left_value: LiteralValue::Int(5),
            operator: "+",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "5 - 5;",
            left_value: LiteralValue::Int(5),
            operator: "-",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "5 * 5;",
            left_value: LiteralValue::Int(5),
            operator: "*",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "5 / 5;",
            left_value: LiteralValue::Int(5),
            operator: "/",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "5 > 5;",
            left_value: LiteralValue::Int(5),
            operator: ">",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "5 < 5;",
            left_value: LiteralValue::Int(5),
            operator: "<",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "5 == 5;",
            left_value: LiteralValue::Int(5),
            operator: "==",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "5 != 5;",
            left_value: LiteralValue::Int(5),
            operator: "!=",
            right_value: LiteralValue::Int(5),
        },
        Test {
            input: "true == true",
            left_value: LiteralValue::Bool(true),
            operator: "==",
            right_value: LiteralValue::Bool(true),
        },
    ];

    for test in tests {
        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(&parser);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        let expr_stmt = cast_variant!(stmt, Statement::Expr).unwrap();
        let expr = &*expr_stmt.expr.as_ref().unwrap();
        let infix_expr = cast_variant!(&**expr, Expression::InfixExpr).unwrap();

        assert_eq!(test.operator, infix_expr.operator.literal);
        let left_expr = &*infix_expr.left_expr;
        test_literal_expression(left_expr, &test.left_value).unwrap();

        let right_expr = &*infix_expr.right_expr.as_ref().unwrap();
        test_literal_expression(right_expr, &test.right_value).unwrap();

        println!("{}", program.string_repr());
    }
}

#[test]
fn test_operator_precedence_parsing() {
    struct Test {
        input: &'static str,
        expected: &'static str,
    }

    let tests = vec![
        Test {
            input: "-a * b",
            expected: "((-a) * b)",
        },
        Test {
            input: "!-a",
            expected: "(!(-a))",
        },
        Test {
            input: "a + b + c",
            expected: "((a + b) + c)",
        },
        Test {
            input: "a + b - c",
            expected: "((a + b) - c)",
        },
        Test {
            input: "a * b * c",
            expected: "((a * b) * c)",
        },
        Test {
            input: "a * b / c",
            expected: "((a * b) / c)",
        },
        Test {
            input: "a + b / c",
            expected: "(a + (b / c))",
        },
        Test {
            input: "a + b * c + d / e - f",
            expected: "(((a + (b * c)) + (d / e)) - f)",
        },
        Test {
            input: "3 + 4; -5 * 5",
            expected: "(3 + 4)((-5) * 5)",
        },
        Test {
            input: "5 > 4 == 3 < 4",
            expected: "((5 > 4) == (3 < 4))",
        },
        Test {
            input: "5 < 4 != 3 > 4",
            expected: "((5 < 4) != (3 > 4))",
        },
        Test {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        // Boolean Expressions
        Test {
            input: "true",
            expected: "true",
        },
        Test {
            input: "false",
            expected: "false",
        },
        Test {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false)",
        },
        Test {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true)",
        },
        // Grouped(Parenthesised) Expressions
        Test {
            input: "1 + (2 + 3) + 4",
            expected: "((1 + (2 + 3)) + 4)",
        },
        Test {
            input: "(5 + 5) * 2",
            expected: "((5 + 5) * 2)",
        },
        Test {
            input: "2 / (5 + 5)",
            expected: "(2 / (5 + 5))",
        },
        Test {
            input: "-(5 + 5)",
            expected: "(-(5 + 5))",
        },
        Test {
            input: "!(true == true)",
            expected: "(!(true == true))",
        },
        Test {
            input: "a + add(b * c) + d",
            expected: "((a + add((b * c))) + d)",
        },
        Test {
            input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        Test {
            input: "add(a + b + c * d / f + g)",
            expected: "add((((a + b) + ((c * d) / f)) + g))",
        },
        // Test {
        //     input: "if (x < y) { x }",
        //     expected: "(if )"
        // }
    ];

    for test in tests {
        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(&parser);

        let actual = program.string_repr();
        assert_eq!(test.expected, actual);
    }
}

fn test_identifier(expr: &Expression, value: &str) -> TResult<()> {
    let ident = cast_variant!(expr, Expression::Ident).or_else(print_error)?;

    check_eq!(value, &ident.value).or_else(print_error)?;
    check_eq!(&ident.token.literal, value)
        .or_else(print_error)
        .unwrap();

    Ok(())
}

/*
 * A few words regarding the mess ahead:
 * In the book most of these functions return a bool, where I decided to return Results. I'm not
 * sure sure what the reason is behind returning upon error instead of panicking, so I'm doing a
 * weird combination of the two right now
 *
 * */

fn test_integer_literal(expr: &Expression, value: &i64) -> TResult<()> {
    let integ = cast_variant!(expr, Expression::Int).or_else(print_error)?;

    check_eq!(integ.value, *value).or_else(print_error)?;

    check_eq!(&integ.token.literal, &value.to_string()).or_else(print_error)?;

    Ok(())
}

fn test_string_literal(expr: &Expression, value: &str) -> TResult<()> {
    let string = cast_variant!(expr, Expression::Ident).or_else(print_error)?;

    check_eq!(&string.value, value).or_else(print_error)?;

    check_eq!(&string.token.literal, value).or_else(print_error)?;

    Ok(())
}

fn test_bool_literal(expr: &Expression, value: &bool) -> TResult<()> {
    let boolean = cast_variant!(expr, Expression::Bool).or_else(print_error)?;

    check_eq!(boolean.value, *value).or_else(print_error)?;

    check_eq!(&boolean.token.literal, &value.to_string()).or_else(print_error)?;

    Ok(())
}

fn test_literal_expression(expr: &Expression, expected: &LiteralValue) -> TResult<()> {
    match expected {
        LiteralValue::Int(n) => test_integer_literal(expr, n),
        LiteralValue::Str(s) => test_string_literal(expr, &s),
        LiteralValue::Bool(b) => test_bool_literal(expr, b),
    }
}

fn test_infix_expression(
    expr: &Expression,
    left: &LiteralValue,
    operator: &str,
    right: &LiteralValue,
) -> TResult<()> {
    let op_expr = cast_variant!(expr, Expression::InfixExpr).or_else(print_error)?;

    test_literal_expression(&*op_expr.left_expr, &left)?;

    check_eq!(operator, &op_expr.operator.literal).or_else(print_error)?;

    test_literal_expression(&*op_expr.right_expr.as_ref().unwrap(), &right)?;

    Ok(())
}

#[test]
fn test_boolean_literal_expression() {
    let input = "
        true;
        false;
        let foobar = true;
        let barfoo = false;
    "
    .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    assert_eq!(4, program.statements.len());
    check_parse_errors(&parser);
}

#[test]
fn test_string_literal_expression() {
    let input = r#""hello world""#.to_string();
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let mut program = parser.parse_program();
    assert_eq!(1, program.statements.len());
    check_parse_errors(&parser);

    let stmt = program.statements.remove(0);
    let expr = cast_variant!(stmt, Statement::Expr).unwrap().expr.unwrap();
    let literal = cast_variant!(*expr, Expression::String).unwrap();
    assert_eq!(literal.value, "hello world")
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parse_errors(&parser);

    assert_eq!(1, program.statements.len());

    let stmt = &cast_variant!(&program.statements[0], Statement::Expr).unwrap();

    let stmt = stmt.expr.as_ref().expect("expected Expression, got None");
    let expr = cast_variant!(&**stmt, Expression::IfExpr).unwrap();

    test_infix_expression(
        &expr.condition.as_ref().unwrap(),
        &LiteralValue::Str("x".to_string()),
        "<",
        &LiteralValue::Str("y".to_string()),
    )
    .unwrap();

    assert_eq!(1, expr.consequence.statements.len());

    let expr = &expr.consequence;
    let consequence = &*expr.statements[0];
    let consequence = cast_variant!(consequence, Statement::Expr).unwrap();

    test_identifier(&consequence.expr.as_ref().unwrap(), "x").unwrap();
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parse_errors(&parser);

    assert_eq!(1, program.statements.len());

    let stmt = &cast_variant!(&program.statements[0], Statement::Expr).unwrap();

    let stmt = stmt.expr.as_ref().expect("expected Expression, got None");
    let expr = cast_variant!(&**stmt, Expression::IfExpr).unwrap();

    test_infix_expression(
        &expr.condition.as_ref().unwrap(),
        &LiteralValue::Str("x".to_string()),
        "<",
        &LiteralValue::Str("y".to_string()),
    )
    .unwrap();

    assert_eq!(1, expr.consequence.statements.len());

    let consequence = &expr.consequence;
    let consequence = &*consequence.statements[0];
    let consequence = cast_variant!(consequence, Statement::Expr).unwrap();

    test_identifier(&consequence.expr.as_ref().unwrap(), "x").unwrap();

    let alt = expr.alternative.as_ref().unwrap().as_ref().unwrap();
    let alt = &*alt.statements[0];
    let alt = cast_variant!(alt, Statement::Expr).unwrap();

    test_identifier(&alt.expr.as_ref().unwrap(), "y").unwrap();
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parse_errors(&parser);

    assert_eq!(1, program.statements.len());
    let expr = cast_variant!(&program.statements[0], Statement::Expr).unwrap();
    let expr = &*expr.expr.as_ref().unwrap();
    let func = cast_variant!(&**expr, Expression::Fn).unwrap();

    let params = func.parameters.as_ref().unwrap();
    assert_eq!(2, params.len());

    let param_1 = params[0].clone();
    // let param_1 = binding.borrow();
    test_literal_expression(
        &Expression::Ident(param_1),
        &LiteralValue::Str("x".to_string()),
    )
    .unwrap();

    let param_2 = params[1].clone();
    test_literal_expression(
        &Expression::Ident(param_2),
        &LiteralValue::Str("y".to_string()),
    )
    .unwrap();
}

#[test]
fn test_function_parameter_parsing() {
    struct Test {
        input: &'static str,
        expected_params: Vec<&'static str>,
    }

    let tests = vec![
        Test {
            input: "fn() {};",
            expected_params: vec![],
        },
        Test {
            input: "fn(x) {};",
            expected_params: vec!["x"],
        },
        Test {
            input: "fn(x, y, z) {};",
            expected_params: vec!["x", "y", "z"],
        },
    ];

    for test in tests {
        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(&parser);

        let stmt = cast_variant!(&program.statements[0], Statement::Expr).unwrap();
        let stmt = stmt.expr.as_ref().unwrap();
        let func = cast_variant!(&**stmt, Expression::Fn).unwrap();

        let params = func.parameters.as_ref().unwrap();
        assert_eq!(test.expected_params.len(), params.len());

        for (expected, got) in test.expected_params.iter().zip(params) {
            let ident = got.clone();
            let expected = LiteralValue::Str(expected.to_string());
            test_literal_expression(&Expression::Ident(ident), &expected).unwrap();
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parse_errors(&parser);

    assert_eq!(1, program.statements.len());

    let stmt = cast_variant!(&program.statements[0], Statement::Expr).unwrap();

    let stmt = &*stmt.expr.as_ref().unwrap();
    let expr = cast_variant!(&**stmt, Expression::Call).unwrap();

    test_literal_expression(&expr.arguments[0].as_ref().unwrap(), &LiteralValue::Int(1)).unwrap();

    test_infix_expression(
        &expr.arguments[1].as_ref().unwrap(),
        &LiteralValue::Int(2),
        "*",
        &LiteralValue::Int(3),
    )
    .unwrap();

    test_infix_expression(
        &expr.arguments[2].as_ref().unwrap(),
        &LiteralValue::Int(4),
        "+",
        &LiteralValue::Int(5),
    )
    .unwrap();
}

#[test]
fn test_array_expression_parsing() {
    let input = "[1, 2 * 2, 3 + 3]";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    check_parse_errors(&parser);

    assert_eq!(1, program.statements.len());

    let stmt = cast_variant!(&program.statements[0], Statement::Expr).unwrap();

    let stmt = &*stmt.expr.as_ref().unwrap();
    let arr_expr = cast_variant!(&**stmt, Expression::Array).unwrap();

    test_literal_expression(
        &arr_expr.elements[0].as_ref().unwrap(),
        &LiteralValue::Int(1),
    )
    .unwrap();

    test_infix_expression(
        &arr_expr.elements[1].as_ref().unwrap(),
        &LiteralValue::Int(2),
        "*",
        &LiteralValue::Int(2),
    )
    .unwrap();

    test_infix_expression(
        &arr_expr.elements[2].as_ref().unwrap(),
        &LiteralValue::Int(3),
        "+",
        &LiteralValue::Int(3),
    )
    .unwrap();
}
