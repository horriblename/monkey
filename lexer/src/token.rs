#[derive(Debug, Clone)]
pub struct Token {
    pub type_: TokenType,
    pub literal: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    Illegal,
    EOF,

    // Identifiers + literals,
    Ident,
    Int,
    If,
    Else,
    Return,
    True,
    False,
    String,

    // Operators,
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    Colon,

    // Comments
    // CommentMarker,
    // CommentString,

    // Delimiters,
    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Function,
    Let,
}
