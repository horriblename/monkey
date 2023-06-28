use std::fmt::Display;

use lexer::{
    lexer::Lexer,
    token::{Token, TokenType},
};

use crate::ast::{self, Expression};

#[derive(Debug)]
pub struct ParseError(String);

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct Parser {
    // TODO reimplement using Lexer.iter(), possibly with Peekable
    lexer: Lexer,
    curr_token: Token,
    peek_token: Token,
    errors: Vec<ParseError>,
    // I bet traits can do whatever we're trying here
    // prefix_parse_fns: HashMap<TokenType, Box<PrefixParseFn>>,
    // infix_parse_fns: HashMap<TokenType, Box<InfixParseFn>>,
}

enum InfixType {
    Op(ast::InfixExpression),
    Call(ast::CallExpression),
    Index(ast::IndexExpression),
    NotInfix(ast::Expression),
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let curr_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let parser = Parser {
            lexer,
            curr_token,
            peek_token,
            errors: vec![],
            // prefix_parse_fns: Default::default(),
            // infix_parse_fns: Default::default(),
        };

        parser
    }

    // fn register_prefix(&mut self, token_type: TokenType, f: Box<PrefixParseFn>) {
    //     self.prefix_parse_fns.insert(token_type, f);
    // }

    fn next_token(&mut self) -> Token {
        let mut curr = self.lexer.next_token();
        std::mem::swap(&mut self.curr_token, &mut self.peek_token);
        std::mem::swap(&mut curr, &mut self.peek_token);
        curr
    }

    fn curr_token_is(&self, t: TokenType) -> bool {
        self.curr_token.type_ == t
    }

    fn _peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.type_ == t
    }

    /// `expect_peek` checks if the currently peeked token is the same type as `t`.
    /// If they are, advance the parser and return `true`, else return `false`.
    fn _expect_peek(&mut self, t: TokenType) -> bool {
        if self._peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn curr_precedence(&self) -> OperatorPrecedence {
        Self::operator_precedence(&self.curr_token.type_)
    }

    fn _peek_precedence(&self) -> OperatorPrecedence {
        Self::operator_precedence(&self.peek_token.type_)
    }

    // NOTE: I find `expect_peek` somewhat unintuitive to use, so I opted for something like this
    // instead
    fn expect_next(&mut self, t: TokenType) -> Option<Token> {
        if self.curr_token.type_ == t {
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
            expected, self.curr_token.type_
        );
        self.errors.push(ParseError(msg));
    }

    fn add_no_prefix_parse_fn_error(&mut self, t: TokenType) {
        let msg = format!("no prefix parse function for {:?} found", t);
        self.errors.push(ParseError(msg));
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };

        while self.curr_token.type_ != TokenType::EOF {
            // might want to turn program.statements into Vec<Option<Statement>>>
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            } else {
                // NOTE: this part is not in the book; for some reason I need this to stop infinite
                // loops
                // FIXME: should turn program.statements into Vec<Option<_>> and push None here
                if self.curr_token_is(TokenType::Semicolon) {
                    self.next_token();
                } else {
                    let tok = self.next_token();
                    self.errors.push(ParseError(format!(
                        "Unexpected token: {}, skipping token",
                        tok.literal
                    )))
                }
            }
        }

        program
    }

    pub fn get_errors(&self) -> &Vec<ParseError> {
        &self.errors
    }

    pub fn take_errors(&mut self) -> Vec<ParseError> {
        std::mem::replace(&mut self.errors, vec![])
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.curr_token.type_ {
            TokenType::Let => Some(ast::Statement::Let(self.parse_let_statement())),
            TokenType::Return => Some(ast::Statement::Return(self.parse_return_statement())),
            _ => self
                .parse_expression_statement()
                .map(|expr| ast::Statement::Expr(expr)),
        }
    }

    fn parse_let_statement(&mut self) -> ast::LetStatement {
        let let_keyword = self.next_token();
        assert_eq!(let_keyword.type_, TokenType::Let);

        let name = self.expect_next(TokenType::Ident).and_then(|tok| {
            let literal = tok.literal.clone();
            Some(Box::new(ast::Identifier {
                token: tok,
                value: literal,
            }))
        });

        self.expect_next(TokenType::Assign);

        let value = self
            .parse_expression(OperatorPrecedence::Lowest)
            .map(Box::new);

        if self.curr_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        ast::LetStatement {
            token: let_keyword,
            name,
            value,
        }
    }

    fn parse_return_statement(&mut self) -> ast::ReturnStatement {
        let return_keyword = self.next_token();
        assert_eq!(return_keyword.type_, TokenType::Return);

        let expr = self
            .parse_expression(OperatorPrecedence::Lowest)
            .map(Box::new);

        if self.curr_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        ast::ReturnStatement {
            token: return_keyword,
            expr, // TODO
        }
    }

    /// `;` is treated as an ExpressionStatement with `None` in the `expr` field.
    /// This function only returns None if no expression or `;` was found.
    fn parse_expression_statement(&mut self) -> Option<ast::ExpressionStatement> {
        let expr = self
            .parse_expression(OperatorPrecedence::Lowest)
            .map(Box::new);

        // NOTE: this is not in the book and I can't remember why I added it :P
        if expr.is_none() && !self.curr_token_is(TokenType::Semicolon) {
            return None;
        }

        if self.curr_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(ast::ExpressionStatement { expr })
    }

    // unary operators always take precedence over other operators, so we check for that first.
    //
    /// Chapter 2.7 gives a nice explanation of how everything works together!
    ///
    /// # operator precedence
    ///
    /// The goal is to have expression operators with higher precedence to be deeper in the tree
    /// than expressions with lower precedence operators. This is accomplished by the `precedence`
    /// argument (again, chapter 2.7, towards the middle part gives a good explanation using
    /// concepts like left/right binding power)
    fn parse_expression(&mut self, precedence: OperatorPrecedence) -> Option<ast::Expression> {
        if let Some(mut left_expr) = self.parse_possible_prefix() {
            while !self.curr_token_is(TokenType::Semicolon) && precedence < self.curr_precedence() {
                left_expr = match self.parse_possible_infix_expression(left_expr) {
                    InfixType::NotInfix(expr) => return Some(expr),
                    InfixType::Call(expr) => ast::Expression::Call(expr),
                    InfixType::Op(infix) => ast::Expression::InfixExpr(infix),
                    InfixType::Index(expr) => ast::Expression::Index(expr),
                };
            }

            return Some(left_expr);
        } else {
            self.add_no_prefix_parse_fn_error(self.curr_token.type_.clone());
            None
        }
    }

    // replaces `Parser.prefixParseFns` in the book
    //
    // Prefix operators are followed by any expression as an operand
    // This name is somewhat misleading imo, should probably rename to something like
    // parse_start_of_expression
    fn parse_possible_prefix(&mut self) -> Option<ast::Expression> {
        match self.curr_token.type_ {
            TokenType::Bang | TokenType::Minus => {
                Some(ast::Expression::PrefixExpr(self.parse_prefix_expression()))
            }
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::LBracket => self.parse_array_literal().map(ast::Expression::Array),
            TokenType::LBrace => self.parse_hash_literal().map(ast::Expression::Hash),
            TokenType::Ident => Some(ast::Expression::Ident(self.parse_identifier())),
            TokenType::Int => Some(ast::Expression::Int(self.parse_int())),
            TokenType::True | TokenType::False => Some(ast::Expression::Bool(self.parse_bool())),
            TokenType::String => {
                let token = self.next_token();
                Some(ast::Expression::String(ast::StringLiteral {
                    value: token.literal.clone(),
                    token,
                }))
            }
            TokenType::If => self
                .parse_if_expression()
                .map(|expr| Expression::IfExpr(expr)),
            TokenType::Function => self
                .parse_fn_literal()
                .map(|func| ast::Expression::Fn(func)),
            _ => None,
        }
    }

    fn parse_prefix_expression(&mut self) -> ast::PrefixExpression {
        let operator = self.next_token();
        let operand = self
            .parse_expression(OperatorPrecedence::Prefix)
            .map(|expr| Box::new(expr));

        ast::PrefixExpression { operator, operand }
    }

    // Parenthesised expressions `(5 + 5)`
    fn parse_grouped_expression(&mut self) -> Option<ast::Expression> {
        self.next_token();
        let expr = self.parse_expression(OperatorPrecedence::Lowest);
        self.expect_next(TokenType::RParen);

        expr
    }

    fn parse_array_literal(&mut self) -> Option<ast::ArrayLiteral> {
        let token = self.next_token();
        let elements = self.parse_expression_list(TokenType::RBracket)?;

        Some(ast::ArrayLiteral { token, elements })
    }

    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Option<ast::Expression>>> {
        let mut elements = vec![];

        if self.curr_token_is(end) {
            self.next_token();
            return Some(elements);
        }

        elements.push(self.parse_expression(OperatorPrecedence::Lowest));

        while self.curr_token_is(TokenType::Comma) {
            self.next_token();
            elements.push(self.parse_expression(OperatorPrecedence::Lowest))
        }

        self.expect_next(TokenType::RBracket)?;

        Some(elements)
    }

    fn parse_hash_literal(&mut self) -> Option<ast::HashLiteral> {
        let token = self.next_token();
        let pairs = self.parse_expression_pairs(TokenType::RBrace)?;

        Some(ast::HashLiteral { token, pairs })
    }

    fn parse_expression_pairs(
        &mut self,
        end: TokenType,
    ) -> Option<Vec<(Option<ast::Expression>, Option<ast::Expression>)>> {
        let mut pairs = vec![];

        if self.curr_token_is(end) {
            self.next_token();
            return Some(pairs);
        }

        let key = self.parse_expression(OperatorPrecedence::Lowest);
        self.expect_next(TokenType::Colon)?;
        let value = self.parse_expression(OperatorPrecedence::Lowest);

        pairs.push((key, value));

        while self.curr_token_is(TokenType::Comma) {
            self.next_token(); // eat comma
            let key = self.parse_expression(OperatorPrecedence::Lowest);
            self.expect_next(TokenType::Colon)?;
            let value = self.parse_expression(OperatorPrecedence::Lowest);

            pairs.push((key, value));
        }

        self.expect_next(TokenType::RBrace);

        Some(pairs)
    }

    // replaces `Parser.prefixParseFns` in the book
    //
    fn parse_possible_infix_expression(&mut self, left: ast::Expression) -> InfixType {
        match &self.curr_token.type_ {
            TokenType::Equal
            | TokenType::NotEqual
            | TokenType::LessThan
            | TokenType::GreaterThan
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Slash
            | TokenType::Asterisk => InfixType::Op(self.parse_infix_expression(left)),
            TokenType::LParen => InfixType::Call(self.parse_call_expression(left)),
            TokenType::LBracket => InfixType::Index(self.parse_index_expression(left)),
            _ => InfixType::NotInfix(left),
        }
    }

    fn parse_infix_expression(&mut self, left: ast::Expression) -> ast::InfixExpression {
        let operator = self.next_token();
        assert!(Self::is_infix_operator(&operator.type_));
        let precedence = Self::operator_precedence(&operator.type_);
        let right = self.parse_expression(precedence);

        ast::InfixExpression {
            left_expr: Box::new(left),
            operator,
            right_expr: right.map(|right| Box::new(right)),
        }
    }

    fn parse_call_expression(&mut self, left: ast::Expression) -> ast::CallExpression {
        let token = self.next_token();
        debug_assert_eq!(token.type_, TokenType::LParen);

        let args = self
            .parse_call_arguments()
            .into_iter()
            .map(|arg| arg.map(Box::new))
            .collect();

        ast::CallExpression {
            token,
            function: Box::new(left),
            arguments: args,
        }
    }

    fn parse_index_expression(&mut self, left: ast::Expression) -> ast::IndexExpression {
        let token = self.next_token();
        debug_assert_eq!(token.type_, TokenType::LBracket);

        let index = self
            .parse_expression(OperatorPrecedence::Lowest)
            .map(Box::new);

        self.expect_next(TokenType::RBracket);

        ast::IndexExpression {
            token,
            left: Box::new(left),
            index,
        }
    }

    fn parse_call_arguments(&mut self) -> Vec<Option<ast::Expression>> {
        let mut args = vec![];

        if self.curr_token_is(TokenType::RParen) {
            self.next_token();
            return args;
        }

        args.push(self.parse_expression(OperatorPrecedence::Lowest));

        while self.curr_token_is(TokenType::Comma) {
            self.next_token();
            // let arg = self.next_token();
            args.push(self.parse_expression(OperatorPrecedence::Lowest));
        }

        self.expect_next(TokenType::RParen);

        args
    }

    fn parse_identifier(&mut self) -> ast::Identifier {
        let token = self.next_token();
        let value = token.literal.clone();

        ast::Identifier { token, value }
    }

    fn parse_int(&mut self) -> ast::IntegerLiteral {
        let token = self.next_token();
        assert_eq!(token.type_, TokenType::Int);
        let value: i64 = token.literal.parse().expect(&format!(
            "token is type {:?} but could not parse content {} as integer",
            TokenType::Int,
            &token.literal
        ));

        ast::IntegerLiteral { token, value }
    }

    fn parse_bool(&mut self) -> ast::BooleanLiteral {
        let token = self.next_token();
        assert!(token.type_ == TokenType::True || token.type_ == TokenType::False);
        let value: bool = token.type_ == TokenType::True;

        ast::BooleanLiteral { token, value }
    }

    fn parse_if_expression(&mut self) -> Option<ast::IfExpression> {
        let token = self.next_token();
        assert_eq!(token.type_, TokenType::If);

        if self.expect_next(TokenType::LParen).is_none() {
            return None;
        }

        let condition = self
            .parse_expression(OperatorPrecedence::Lowest)
            .map(Box::new);

        self.expect_next(TokenType::RParen)?;

        self.expect_next(TokenType::LBrace)?;

        let consequence = self.parse_block_expression();

        let alternative = if self.curr_token_is(TokenType::Else) {
            Some({
                self.next_token();
                self.expect_next(TokenType::LBrace)?;
                Some(self.parse_block_expression())
            })
        } else {
            None
        };

        Some(ast::IfExpression {
            token,
            condition,
            consequence,
            alternative,
        })
    }

    fn parse_fn_literal(&mut self) -> Option<ast::FunctionLiteral> {
        let token = self.next_token();

        self.expect_next(TokenType::LParen)?;

        let parameters = self.parse_function_parameters();

        self.expect_next(TokenType::LBrace)?;

        let body = self.parse_block_expression();

        Some(ast::FunctionLiteral {
            token,
            parameters,
            body,
        })
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<ast::Identifier>> {
        let mut params = Vec::new();

        if self.curr_token_is(TokenType::RParen) {
            self.next_token();
            return Some(params);
        }

        let param = self.next_token();
        let ident = ast::Identifier {
            value: param.literal.clone(),
            token: param,
        };
        params.push(ident);

        while self.curr_token_is(TokenType::Comma) {
            self.next_token();

            if let Some(param) = self.expect_next(TokenType::Ident) {
                let ident = ast::Identifier {
                    value: param.literal.clone(),
                    token: param,
                };
                params.push(ident);
            }
        }

        self.expect_next(TokenType::RParen)?;

        Some(params)
    }

    fn parse_block_expression(&mut self) -> ast::BlockStatement {
        let mut statements = vec![];

        while !self.curr_token_is(TokenType::RBrace) {
            if let Some(stmt) = self.parse_maybe_statement() {
                statements.push(Box::new(stmt));
            } else {
                break;
            }
        }

        // probably should make this function Option<_> and propagate this
        self.expect_next(TokenType::RBrace);

        ast::BlockStatement { statements }
    }

    // might be a good idea to merge with `parse_statement`?
    /// returns a statement or None if no statement can be parsed
    fn parse_maybe_statement(&mut self) -> Option<ast::Statement> {
        match self.curr_token.type_ {
            TokenType::Let => Some(ast::Statement::Let(self.parse_let_statement())),
            TokenType::Return => Some(ast::Statement::Return(self.parse_return_statement())),
            _ => self
                .parse_expression_statement()
                .map(|expr| ast::Statement::Expr(expr)),
        }
    }

    fn operator_precedence(token_type: &TokenType) -> OperatorPrecedence {
        match token_type {
            TokenType::LParen => OperatorPrecedence::Call,
            TokenType::LBracket => OperatorPrecedence::Index,
            TokenType::Equal | TokenType::NotEqual => OperatorPrecedence::Equals,
            TokenType::LessThan | TokenType::GreaterThan => OperatorPrecedence::LessGreater,
            TokenType::Plus | TokenType::Minus => OperatorPrecedence::Sum,
            TokenType::Slash | TokenType::Asterisk => OperatorPrecedence::Product,
            _ => OperatorPrecedence::Lowest,
        }
    }

    fn is_infix_operator(token_type: &TokenType) -> bool {
        match token_type {
            TokenType::Equal
            | TokenType::NotEqual
            | TokenType::LessThan
            | TokenType::GreaterThan
            | TokenType::Plus
            | TokenType::Minus
            | TokenType::Slash
            | TokenType::Asterisk => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, PartialOrd)]
enum OperatorPrecedence {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // > or <
    Sum = 3,         // +
    Product = 4,     // *
    Prefix = 5,      // -X or !X
    Call = 6,        // myFunction(X)
    Index = 7,       // array[index]
}

#[cfg(test)]
mod tests;
