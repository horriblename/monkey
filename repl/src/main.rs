mod args;

const PROMPT: &str = "> ";

use lexer::lexer;
use parser::{ast, parse};
use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

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
    let mut env = evaluator::object::EnvStack::new();

    let config = args::read_args().unwrap();

    if let Some(in_file) = config.in_file {
        include_file(in_file, &mut env).unwrap();
    }

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

        let evaluated = evaluator::eval::eval(ast::Node::Prog(program), &mut env);
        match &evaluated {
            Err(err) => eprintln!("Error: {:?}", err),
            Ok(evaluated) => println!("{}", evaluated.borrow().inspect()),
        }
    }
}

fn include_file(file: PathBuf, env: &mut evaluator::object::EnvStack) -> Result<(), RError> {
    let contents = fs::read_to_string(file).map_err(RError::Io)?;

    let lexer = lexer::Lexer::new(contents);
    let mut parser = parse::Parser::new(lexer);
    let program = parser.parse_program();

    let parse_errors = parser.take_errors();
    if parse_errors.len() > 0 {
        print_parse_errors(&parse_errors);
        return Err(RError::Parser(parse_errors));
    }

    let evaluated = evaluator::eval::eval(ast::Node::Prog(program), env);
    match &evaluated {
        Err(err) => eprintln!("Error: {:?}", err),
        Ok(evaluated) => println!("{}", evaluated.borrow().inspect()),
    }

    Ok(())
}

#[derive(Debug)]
enum RError {
    Parser(Vec<parser::parse::ParseError>),
    Io(io::Error),
}
