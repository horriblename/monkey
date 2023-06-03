use std::{cell::RefCell, rc::Rc};

use lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};

use crate::ast;

struct Parser {
    // TODO reimplement using Lexer.iter(), possibly with Peekable
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
}

impl Parser {
    fn new(mut lexer: Lexer) -> Self {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            curr_token,
            peek_token,
        }
    }

    fn next_token(&mut self) -> Token {
        let mut curr = self.lexer.next_token();
        std::mem::swap(&mut self.curr_token, &mut self.peek_token);
        std::mem::swap(&mut curr, &mut self.peek_token);
        curr
    }

    fn curr_token_is(&self, t: TokenType) -> bool {
        self.curr_token.token_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };

        while self.curr_token.token_type != TokenType::EOF {
            let statement = self.parse_statement();
            program.statements.push(statement);
        }

        program
    }

    fn parse_statement(&mut self) -> ast::Statement {
        match self.curr_token.token_type {
            TokenType::Let => ast::Statement::Let(self.parse_let_statement()),
            _ => todo!("error handling"),
        }
    }

    fn parse_let_statement(&mut self) -> ast::LetStatement {
        let let_keyword = self.next_token();
        assert_eq!(let_keyword.token_type, TokenType::Let);

        let variable_name = self.next_token();
        if variable_name.token_type != (TokenType::Ident) {
            todo!("error handling");
        }
        let variable_literal = variable_name.literal.clone();

        if self.next_token().token_type != TokenType::Assign {
            todo!("error handling");
        }

        // TODO: we're skipping the expressions until we encounter a semicolon
        while self.next_token().token_type != TokenType::Semicolon {}

        ast::LetStatement {
            token: let_keyword,
            name: Rc::new(RefCell::new(ast::Identifier {
                token: variable_name,
                value: variable_literal,
            })),
            value: Rc::new(RefCell::new(ast::Expression::TempDummy)),
        }
    }
}

#[cfg(test)]
mod tests;
