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
            assert_eq!(test.expected_identifier, let_statement.name.borrow().value);
            assert!(test_let_statement(stmt, test.expected_identifier))
        } else {
            panic!("expected let statement, got something else");
        }
    }
}

fn test_let_statement(stmt: Statement, name: &str) -> bool {
    if let Statement::Let(let_stmt) = &stmt {
        assert_eq!("let", let_stmt.token.literal);

        let name_node = let_stmt.name.borrow();
        assert_eq!(name, name_node.token.literal);
        true
    } else {
        panic!("expected let statement, got something else")
    }
}
