use super::Parser;
use crate::ast::{self, LetStatement, Statement};
use lexer::lexer::Lexer;

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
fn test_let_statement_error() {
    let input = "
        let x 5;
        let = 10;
        let 838383;
        "
    .to_string();
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let _program = parser.parse_program();

    check_parse_errors(&parser)
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
