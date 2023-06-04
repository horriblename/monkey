// A lexer will take source code as input and output the tokens that represent the source code. It
// will go through its input and output the next token it recognizes. It doesn't need to buffer or
// save tokens, since there will only be one method called `NextToken()`, which will output the
// next token.

use crate::token::{Token, TokenType};

// NOTE: would probably be easier to turn input into char iterator
#[derive(Debug)]
pub struct Lexer {
    // NOTE: the book ignores utf-8 stuff; to match it as much as possible, I'm using u8, will
    // possibly rewrite later
    input: Vec<u8>,
    position: u32,
    read_position: u32,
    char: Option<u8>, // current char under examination (peek)
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input: input.into_bytes(),
            position: 0,
            read_position: 0,
            char: None,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        let new_token = |token_type, ch: char| Token {
            type_: token_type,
            literal: ch.to_string(),
        };

        self.skip_whitespace();

        let tok = match self.char {
            Some(b';') => new_token(TokenType::Semicolon, ';'),
            Some(b'(') => new_token(TokenType::LParen, '('),
            Some(b')') => new_token(TokenType::RParen, ')'),
            Some(b',') => new_token(TokenType::Comma, ','),
            Some(b'+') => new_token(TokenType::Plus, '+'),
            Some(b'-') => new_token(TokenType::Minus, '-'),
            Some(b'*') => new_token(TokenType::Asterisk, '*'),
            Some(b'<') => new_token(TokenType::LessThan, '<'),
            Some(b'>') => new_token(TokenType::GreaterThan, '>'),
            Some(b'{') => new_token(TokenType::LBrace, '{'),
            Some(b'}') => new_token(TokenType::RBrace, '}'),
            Some(b'/') => new_token(TokenType::Slash, '/'),
            Some(b'!') => {
                if let Some(b'=') = self.peek_char() {
                    self.read_char();
                    Token {
                        type_: TokenType::NotEqual,
                        literal: "!=".to_string(),
                    }
                } else {
                    new_token(TokenType::Bang, '!')
                }
            }
            Some(b'=') => {
                if let Some(b'=') = self.peek_char() {
                    self.read_char();
                    Token {
                        type_: TokenType::Equal,
                        literal: "==".to_string(),
                    }
                } else {
                    new_token(TokenType::Assign, '=')
                }
            }
            Some(ch) if ch.is_ascii_digit() => {
                return Token {
                    type_: TokenType::Int,
                    literal: self.read_int(),
                };
            }
            Some(ch) if ch.is_ascii_alphabetic() => {
                let literal = self.read_identifier();
                return Token {
                    type_: Self::lookup_ident(&literal),
                    literal,
                };
            }
            Some(_) => Token {
                type_: TokenType::Illegal,
                literal: "".to_string(),
            },
            None => Token {
                type_: TokenType::EOF,
                literal: "".to_string(),
            },
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() as u32 {
            self.char = None;
        } else {
            self.char = Some(self.input[self.read_position as usize]);
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    /// Peek at the next character. Returns `None` at end of file.
    fn peek_char(&self) -> Option<u8> {
        if self.read_position >= self.input.len() as u32 {
            None
        } else {
            Some(self.input[self.read_position as usize])
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        // read as long as self.ch is alphabetic
        while self.char.filter(|ch| ch.is_ascii_alphabetic()).is_some() {
            self.read_char();
        }

        // why can't I use slice??
        // self.input[position .. self.position]
        String::from_utf8(self.input[position as usize..(self.position) as usize].to_vec())
            .expect("unreachable: all chars are valid alphabets")
    }

    fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "let" => TokenType::Let,
            "fn" => TokenType::Function,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            _ => TokenType::Ident,
        }
    }

    fn read_int(&mut self) -> String {
        let position = self.position;

        while self.char.filter(|ch| ch.is_ascii_digit()).is_some() {
            self.read_char();
        }

        String::from_utf8(self.input[position as usize..(self.position) as usize].to_vec())
            .expect("unreachable: all chars are digits")
    }

    fn skip_whitespace(&mut self) {
        while self.char.is_some() && self.char.unwrap().is_ascii_whitespace() {
            self.read_char();
        }

        assert!(self.char.is_none() || !self.char.unwrap().is_ascii_whitespace());
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, token::TokenType};

    #[test]
    fn test_next_token() {
        use TokenType::*;
        let input = "let five = 5;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;

            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
            ";

        struct Expected {
            tok_type: TokenType,
            literal: &'static str,
        }
        let expect = |tok_type, literal| Expected { tok_type, literal };

        let tests: Vec<Expected> = vec![
            expect(Let, "let"),
            expect(Ident, "five"),
            expect(Assign, "="),
            expect(Int, "5"),
            expect(Semicolon, ";"),
            expect(Let, "let"),
            expect(Ident, "ten"),
            expect(Assign, "="),
            expect(Int, "10"),
            expect(Semicolon, ";"),
            expect(Let, "let"),
            expect(Ident, "add"),
            expect(Assign, "="),
            expect(Function, "fn"),
            expect(LParen, "("),
            expect(Ident, "x"),
            expect(Comma, ","),
            expect(Ident, "y"),
            expect(RParen, ")"),
            expect(LBrace, "{"),
            expect(Ident, "x"),
            expect(Plus, "+"),
            expect(Ident, "y"),
            expect(Semicolon, ";"),
            expect(RBrace, "}"),
            expect(Semicolon, ";"),
            expect(Let, "let"),
            expect(Ident, "result"),
            expect(Assign, "="),
            expect(Ident, "add"),
            expect(LParen, "("),
            expect(Ident, "five"),
            expect(Comma, ","),
            expect(Ident, "ten"),
            expect(RParen, ")"),
            expect(Semicolon, ";"),
            expect(Bang, "!"),
            expect(Minus, "-"),
            expect(Slash, "/"),
            expect(Asterisk, "*"),
            expect(Int, "5"),
            expect(Semicolon, ";"),
            expect(Int, "5"),
            expect(LessThan, "<"),
            expect(Int, "10"),
            expect(GreaterThan, ">"),
            expect(Int, "5"),
            expect(Semicolon, ";"),
            expect(If, "if"),
            expect(LParen, "("),
            expect(Int, "5"),
            expect(LessThan, "<"),
            expect(Int, "10"),
            expect(RParen, ")"),
            expect(LBrace, "{"),
            expect(Return, "return"),
            expect(True, "true"),
            expect(Semicolon, ";"),
            expect(RBrace, "}"),
            expect(Else, "else"),
            expect(LBrace, "{"),
            expect(Return, "return"),
            expect(False, "false"),
            expect(Semicolon, ";"),
            expect(RBrace, "}"),
            expect(Int, "10"),
            expect(Equal, "=="),
            expect(Int, "10"),
            expect(Semicolon, ";"),
            expect(Int, "10"),
            expect(NotEqual, "!="),
            expect(Int, "9"),
            expect(Semicolon, ";"),
            expect(EOF, ""),
        ];

        let mut lexer = Lexer::new(input.to_string());

        for (i, test) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            println!("test #{}: got token: {}", i, tok.literal);
            assert_eq!(test.tok_type, tok.type_);
            assert_eq!(test.literal, tok.literal);
        }
    }
}
