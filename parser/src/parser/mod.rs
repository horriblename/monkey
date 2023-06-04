#![allow(dead_code)]

use std::{cell::RefCell, collections::HashMap, rc::Rc};

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

    // I bet traits can do whatever we're trying here
    prefix_parse_fns: HashMap<TokenType, Box<PrefixParseFn>>,
    infix_parse_fns: HashMap<TokenType, Box<InfixParseFn>>,
}

type PrefixParseFn = dyn Fn() -> ast::Expression;

// takes one argument: the "left side" of the infix operator
type InfixParseFn = dyn Fn(ast::Expression) -> ast::Expression;

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let parser = Parser {
            lexer,
            curr_token,
            peek_token,
            errors: vec![],
            prefix_parse_fns: Default::default(),
            infix_parse_fns: Default::default(),
        };

        parser
    }

    // fn register_prefix(&mut self, _token_type: TokenType, _f: Box<PrefixParseFn>) {
    //     // self.prefix_parse_fns.insert(token_type, f);
    // }

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
            expected, self.curr_token.token_type
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
            _ => ast::Statement::Expr(self.parse_expression_statement()),
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

    fn parse_expression_statement(&mut self) -> ast::ExpressionStatement {
        let expr = Rc::new(RefCell::new(
            self.parse_expression(OperatorPrecedence::Lowest),
        ));

        if self.curr_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        ast::ExpressionStatement { expr }
    }

    fn parse_expression(&mut self, _precedence: OperatorPrecedence) -> ast::Expression {
        if let Some(prefix) = self.parse_possible_prefix() {
            prefix
        } else {
            // TODO
            self.next_token();
            ast::Expression::TempDummy
        }
    }

    // replaces `Parser.prefixParseFns` in the book
    //
    // Prefix operators are followed by any expression as an operand
    fn parse_possible_prefix(&mut self) -> Option<ast::Expression> {
        match self.curr_token.token_type {
            TokenType::Bang | TokenType::Minus => {
                Some(ast::Expression::PrefixExpr(self.parse_prefix_expression()))
            }
            TokenType::Ident => Some(ast::Expression::Ident(self.parse_identifier())),
            TokenType::Int => Some(ast::Expression::Int(self.parse_int())),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> ast::PrefixExpression {
        let operator = self.next_token();
        let operand = Rc::new(RefCell::new(
            self.parse_expression(OperatorPrecedence::Prefix),
        ));

        ast::PrefixExpression { operator, operand }
    }

    fn add_no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(ParseError(msg));
    }

    fn parse_identifier(&mut self) -> ast::Identifier {
        let token = self.next_token();
        let value = token.literal.clone();

        ast::Identifier { token, value }
    }

    fn parse_int(&mut self) -> ast::IntegerLiteral {
        let token = self.next_token();
        assert_eq!(token.token_type, TokenType::Int);
        let value: i64 = token.literal.parse().expect(&format!(
            "token is type {:?} but could not parse content {} as integer",
            TokenType::Int,
            &token.literal
        ));

        ast::IntegerLiteral { token, value }
    }
}

enum OperatorPrecedence {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // > or <
    Sum = 3,         // +
    Product = 4,     // *
    Prefix = 5,      // -X or !X
    Call = 6,        // myFunction(X)
}

#[cfg(test)]
mod tests;
