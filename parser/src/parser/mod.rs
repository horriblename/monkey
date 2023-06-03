#![allow(dead_code)]

use std::{cell::RefCell, rc::Rc};

use lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};

use crate::ast;

#[derive(Debug)]
struct ParseError(String);

struct Parser {
    // TODO reimplement using Lexer.iter(), possibly with Peekable
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            curr_token,
            peek_token,
            errors: vec![],
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

    /// `expect_peek` checks if the currently peeked token is the same type as `t`.
    /// If they are, advance the parser and return `true`, else return `false`.
    fn _expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }

    // NOTE: I find `expect_peek` somewhat unintuitive to use, so I opted for something like this
    // instead
    fn expect_next(&mut self, t: TokenType) -> Option<Token> {
        if self.curr_token.token_type == t {
            Some(self.next_token())
        } else {
            self.add_next_error(t);
            None
        }
    }

    // NOTE: I'm getting different errors from the book, most likely due to them checking with
    // peeks instead of my next_...() calls; moving on until I get more context
    fn add_next_error(&mut self, expected: TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            expected, self.peek_token.token_type
        );
        self.errors.push(ParseError(msg));
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };

        while self.curr_token.token_type != TokenType::EOF {
            let statement = self.parse_statement();
            program.statements.push(statement);
        }

        program
    }

    pub fn get_errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    fn parse_statement(&mut self) -> ast::Statement {
        match self.curr_token.token_type {
            TokenType::Let => ast::Statement::Let(self.parse_let_statement()),
            TokenType::Return => ast::Statement::Return(self.parse_return_statement()),
            _ => todo!("error handling"),
        }
    }

    fn parse_let_statement(&mut self) -> ast::LetStatement {
        let let_keyword = self.next_token();
        assert_eq!(let_keyword.token_type, TokenType::Let);

        let name_token = self.expect_next(TokenType::Ident).and_then(|tok| {
            let literal = tok.literal.clone();
            Some(Rc::new(RefCell::new(ast::Identifier {
                token: tok,
                value: literal,
            })))
        });

        self.expect_next(TokenType::Assign);

        // TODO: we're skipping the expressions until we encounter a semicolon
        while !self.curr_token_is(TokenType::Semicolon) {
            if self.curr_token_is(TokenType::EOF) {
                let msg = format!("expected token or semicolon, got {:?}", TokenType::EOF);
                self.errors.push(ParseError(msg));
                break;
            }
            self.next_token();
        }
        self.next_token();

        ast::LetStatement {
            token: let_keyword,
            name: name_token,
            value: Rc::new(RefCell::new(ast::Expression::TempDummy)),
        }
    }

    fn parse_return_statement(&mut self) -> ast::ReturnStatement {
        let return_keyword = self.next_token();
        assert_eq!(return_keyword.token_type, TokenType::Return);

        // TODO: we're skipping the expressions until we encounter a semicolon
        while !self.curr_token_is(TokenType::Semicolon) {
            if self.curr_token_is(TokenType::EOF) {
                let msg = format!("expected token or semicolon, got {:?}", TokenType::EOF);
                self.errors.push(ParseError(msg));
                break;
            }
            self.next_token();
        }
        self.next_token();

        ast::ReturnStatement {
            token: return_keyword,
            expr: Rc::new(RefCell::new(ast::Expression::TempDummy)),
        }
    }
}

#[cfg(test)]
mod tests;
