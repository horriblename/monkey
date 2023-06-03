use super::Parser;
use crate::ast::{Expression, LetStatement, Statement};
use lexer::{lexer::Lexer, token::TokenType};

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
fn test_print() {
    use crate::ast::representation::StringRepr;

    let input = "
        let x = 5;
        let y = 10;
        let foobar = 838383;
    "
    .to_string();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    println!("{}", program.string_repr());
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
        if let Expression::Ident(ident) = &expr.expr {
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
        if let Expression::Int(integer) = &expr.expr {
            assert_eq!(5, integer.value);
            assert_eq!("5", integer.token.literal);
        } else {
            panic!("expected Expression::Ident, got {:?}", expr);
        }
    } else {
        panic!("expected ExpressionStatement, got something else");
    }
}
