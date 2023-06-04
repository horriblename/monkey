use std::{cell::RefCell, rc::Rc};

use super::Parser;
use crate::ast::{
    representation::StringRepr, Expression, Identifier, LetStatement, Program, Statement,
};
use lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};

macro_rules! unwrap_variant {
    ($target: expr, $pat: path) => {{
        if let $pat(a) = $target {
            a
        } else {
            panic!("mismatch variant when cast to {}", stringify!($pat)); // #2
        }
    }};
}

#[test]
fn test_let_statements() {
    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    "
    .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    assert_eq!(3, program.statements.len());
    check_parse_errors(&parser);

    struct Test {
        expected_identifier: &'static str,
    }
    let tests = vec![
        Test {
            expected_identifier: "x",
        },
        Test {
            expected_identifier: "y",
        },
        Test {
            expected_identifier: "foobar",
        },
    ];

    for (test, stmt) in tests.iter().zip(program.statements) {
        if let Statement::Let(let_statement) = &stmt {
            assert_eq!(
                test.expected_identifier,
                let_statement.name.as_ref().unwrap().borrow().value
            );
            assert!(test_let_statement(let_statement, test.expected_identifier))
        } else {
            panic!("expected let statement, got something else");
        }
    }
}

#[test]
fn test_return_statements() {
    let input = "
        return 5;
        return 10;
        return 993322;
        "
    .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();
    check_parse_errors(&parser);

    for stmt in program.statements {
        if let Statement::Return(return_statement) = &stmt {
            // TODO test return_statement.expr
            assert_eq!(TokenType::Return, return_statement.token.token_type);
            assert_eq!("return", return_statement.token.literal);
        } else {
            panic!("expected return statemnt, got something else");
        }
    }
}

fn test_let_statement(stmt: &LetStatement, name: &str) -> bool {
    assert_eq!("let", stmt.token.literal);

    let name_node = stmt.name.as_ref().unwrap().borrow();
    assert_eq!(name, name_node.token.literal);
    true
}

fn check_parse_errors(parser: &Parser) {
    // TODO
    for i in &parser.errors {
        println!("Praser error: {:?}", i);
    }

    assert!(parser.errors.is_empty());
}

#[test]
fn test_string_repr() {
    use crate::ast::representation::StringRepr;

    let program = Program {
        statements: vec![Statement::Let(LetStatement {
            token: Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            name: Some(Rc::new(RefCell::new(Identifier {
                token: Token {
                    token_type: TokenType::Ident,
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            }))),
            value: Rc::new(RefCell::new(Expression::Ident(Identifier {
                token: Token {
                    token_type: TokenType::Ident,
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
        if let Expression::Ident(ident) = &*expr.expr.borrow() {
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
        if let Expression::Int(integer) = &*expr.expr.borrow() {
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
        let expr = &*unwrap_variant!(stmt, Statement::Expr).expr.borrow();

        let prefix_expr = unwrap_variant!(expr, Expression::PrefixExpr);

        assert_eq!(t.operator, prefix_expr.operator.literal);

        let right_expr = &*prefix_expr.operand.borrow();
        let rvalue = unwrap_variant!(right_expr, Expression::Int);

        assert_eq!(t.integer_value, rvalue.value);
    }
}

#[test]
fn test_parsing_infix_expressions() {
    struct Test {
        input: &'static str,
        left_value: i64,
        operator: &'static str,
        right_value: i64,
    }

    let tests = vec![
        Test {
            input: "5 + 5;",
            left_value: 5,
            operator: "+",
            right_value: 5,
        },
        Test {
            input: "5 - 5;",
            left_value: 5,
            operator: "-",
            right_value: 5,
        },
        Test {
            input: "5 * 5;",
            left_value: 5,
            operator: "*",
            right_value: 5,
        },
        Test {
            input: "5 / 5;",
            left_value: 5,
            operator: "/",
            right_value: 5,
        },
        Test {
            input: "5 > 5;",
            left_value: 5,
            operator: ">",
            right_value: 5,
        },
        Test {
            input: "5 < 5;",
            left_value: 5,
            operator: "<",
            right_value: 5,
        },
        Test {
            input: "5 == 5;",
            left_value: 5,
            operator: "==",
            right_value: 5,
        },
        Test {
            input: "5 != 5;",
            left_value: 5,
            operator: "!=",
            right_value: 5,
        },
    ];

    for test in tests {
        let lexer = Lexer::new(test.input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        check_parse_errors(&parser);

        assert_eq!(1, program.statements.len());

        let stmt = &program.statements[0];
        let expr_stmt = unwrap_variant!(stmt, Statement::Expr);
        let expr = &*expr_stmt.expr.borrow();
        let infix_expr = unwrap_variant!(expr, Expression::InfixExpr);

        assert_eq!(test.operator, infix_expr.operator.literal);
        let left_expr = &*infix_expr.left_expr.borrow();
        let left_value = unwrap_variant!(left_expr, Expression::Int);

        assert_eq!(test.left_value, left_value.value);

        let right_expr = &*infix_expr.right_expr.borrow();
        let right_value = unwrap_variant!(right_expr, Expression::Int);

        assert_eq!(test.right_value, right_value.value);

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
