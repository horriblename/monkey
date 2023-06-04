// We defined `TokenType` as a String, to allow us to use many different values as `TokenTypes`,
// which in turn allows us to distinguish between different types of tokens.
//
// Using string also has the advantage of being easy to debug without a lot of boilerplate and
// helper functions
//
// Using strings is not as performant as int or byte though

// NOTE: probably would be nicer to rewrite Token as an enum
#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}

// NOTE: might bite me in the ass later: rewrote TokenType as an enum
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

    Function,
    Let,
}

// pub type TokenType = str;
// pub const ILLEGAL: &TokenType = "ILLEGAL"; // token/character we don't know about
// pub const EOF: TokenType = "EOF";
//
// // Identifiers + literals
// pub const IDENT: TokenType = "IDENT"; // variable names
// pub const INT: TokenType = "INT"; // 12345
//
// // Operators
// pub const ASSIGN: TokenType = "=";
// pub const PLUS: TokenType = "+";
//
// // Delimiters
// pub const COMMA: TokenType = ",";
// pub const SEMICOLON: TokenType = ";";
//
// pub const LPAREN: TokenType = "(";
// pub const RPAREN: TokenType = ")";
// pub const LBRACE: TokenType = "{";
// pub const RBRACE: TokenType = "}";
//
// pub const FUNCTION: TokenType = "FUNCTION";
// pub const LET: TokenType = "LET";
