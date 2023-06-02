const PROMPT: &str = "> ";
use lexer::lexer;
use std::io::{self, Write};

#[inline]
fn stdin_next_line() -> io::Result<String> {
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer)?;
    Ok(buffer)
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

        for tok in lexer {
            println!("{:?}", tok)
        }
    }
}
