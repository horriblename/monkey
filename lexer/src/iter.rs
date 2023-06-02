use crate::{
    lexer::Lexer,
    token::{Token, TokenType},
};

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.token_type == TokenType::EOF {
            None
        } else {
            Some(tok)
        }
    }
}
