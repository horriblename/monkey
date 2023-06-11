const PROMPT: &str = "> ";

use lexer::lexer;
use parser::{ast, parse};
use std::io::{self, Write};

#[inline]
fn stdin_next_line() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    Ok(buffer)
}

fn print_parse_errors(errors: &Vec<parse::ParseError>) {
    eprintln!("parse errors:");
    for err in errors {
        eprintln!("\t{}", err);
    }
}

fn main() {
    println!("Welcome to the Monkey programming language!");

    loop {
        print!("{}", PROMPT);

        if let Err(err) = io::stdout().flush() {
            eprintln!("Error flushing stdout: {}", err);
            break;
        }

        let command_result = stdin_next_line();
        if let Err(err) = command_result {
            eprintln!("Error reading stdin: {}", err);
            break;
        }
        let command = command_result.unwrap();

        let lexer = lexer::Lexer::new(command);
        let mut parser = parse::Parser::new(lexer);
        let program = parser.parse_program();

        let parse_errors = parser.get_errors();
        if parse_errors.len() > 0 {
            print_parse_errors(parse_errors);
            continue;
        }

        let evaluated = evaluator::eval::eval(&ast::Node::Prog(program));
        println!("{}", evaluated.inspect());
    }
}
