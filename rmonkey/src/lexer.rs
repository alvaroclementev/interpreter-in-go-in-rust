#![allow(dead_code)]

use crate::token::{Token, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;

        self.skip_whitespace();

        // NOTE(alvaro): This is used instead of the early return that the
        // go code uses
        let mut has_consumed = false;

        // TODO(alvaro): Make a `make_two_char_token` that encapsulates the
        // shared logic between `==` and `!=` branches
        let (kind, literal) = match self.ch {
            '=' => {
                if self.peak_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    (Eq, format!("{}{}", ch, self.ch))
                } else {
                    (Assign, self.ch.to_string())
                }
            }
            '+' => (Plus, self.ch.to_string()),
            '-' => (Minus, self.ch.to_string()),
            '!' => {
                if self.peak_char() == '=' {
                    let ch = self.ch;
                    self.read_char();
                    (NotEq, format!("{}{}", ch, self.ch))
                } else {
                    (Bang, self.ch.to_string())
                }
            }
            '/' => (Slash, self.ch.to_string()),
            '*' => (Asterisk, self.ch.to_string()),
            '<' => (Lt, self.ch.to_string()),
            '>' => (Gt, self.ch.to_string()),
            ':' => (Colon, self.ch.to_string()),
            ';' => (Semicolon, self.ch.to_string()),
            ',' => (Comma, self.ch.to_string()),
            '(' => (LParen, self.ch.to_string()),
            ')' => (RParen, self.ch.to_string()),
            '{' => (LBrace, self.ch.to_string()),
            '}' => (RBrace, self.ch.to_string()),
            '[' => (LBracket, self.ch.to_string()),
            ']' => (RBracket, self.ch.to_string()),
            '\0' => (Eof, "".to_string()),
            '"' => (String, self.read_string()),
            ch if is_letter(ch) => {
                let literal = self.read_identifier();
                let kind = lookup_identifier(&literal);
                has_consumed = true;
                (kind, literal)
            }
            ch if is_digit(ch) => {
                let literal = self.read_number();
                has_consumed = true;
                (Int, literal)
            }
            _ => {
                println!("Found illegal character: {}", self.ch);
                (Illegal, self.ch.to_string())
            }
        };
        if !has_consumed {
            self.read_char();
        }

        Token::new(kind, literal)
    }

    fn read_char(&mut self) {
        self.ch = self.input.chars().nth(self.read_position).unwrap_or('\0');
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peak_char(&self) -> char {
        self.input.chars().nth(self.read_position).unwrap_or('\0')
    }

    // FIXME(alvaro): We can make a `read_while` helper that takes a predicate
    // and unify the `read_number` and `read_identifier` logic
    fn read_identifier(&mut self) -> String {
        let position = self.position;

        while is_letter(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;

        while is_digit(self.ch) {
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;

        // TODO(alvaro): Add support for escaping
        // TODO(alvaro): Add error message for unterminated string literal
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    fn skip_whitespace(&mut self) {
        // FIXME(alvaro): This is superseeded by `char::is_whitespace` builtin
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char()
        }
    }
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_lowercase() || ch.is_ascii_uppercase() || ch == '_'
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn lookup_identifier(ident: &str) -> TokenKind {
    // FIXME(alvaro): This could be a `From` implementation
    match ident {
        "fn" => TokenKind::Function,
        "let" => TokenKind::Let,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "return" => TokenKind::Return,
        _ => TokenKind::Identifier,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token_simple() {
        let input = "=+(){},;".to_string();

        let tests = vec![
            (TokenKind::Assign, "="),
            (TokenKind::Plus, "+"),
            (TokenKind::LParen, "("),
            (TokenKind::RParen, ")"),
            (TokenKind::LBrace, "{"),
            (TokenKind::RBrace, "}"),
            (TokenKind::Comma, ","),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);

        for expected in tests {
            let token = lexer.next_token();
            assert_eq!(token.kind, expected.0);
            assert_eq!(token.literal, expected.1);
        }
    }

    #[test]
    fn test_next_token() {
        let input = r#"let five = 5;
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
"foobar"
"foo bar"
[1, 2];
{"foo": "bar"}
        "#
        .to_string();

        let tests = vec![
            (TokenKind::Let, "let"),
            (TokenKind::Identifier, "five"),
            (TokenKind::Assign, "="),
            (TokenKind::Int, "5"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Let, "let"),
            (TokenKind::Identifier, "ten"),
            (TokenKind::Assign, "="),
            (TokenKind::Int, "10"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Let, "let"),
            (TokenKind::Identifier, "add"),
            (TokenKind::Assign, "="),
            (TokenKind::Function, "fn"),
            (TokenKind::LParen, "("),
            (TokenKind::Identifier, "x"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "y"),
            (TokenKind::RParen, ")"),
            (TokenKind::LBrace, "{"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Plus, "+"),
            (TokenKind::Identifier, "y"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::RBrace, "}"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Let, "let"),
            (TokenKind::Identifier, "result"),
            (TokenKind::Assign, "="),
            (TokenKind::Identifier, "add"),
            (TokenKind::LParen, "("),
            (TokenKind::Identifier, "five"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "ten"),
            (TokenKind::RParen, ")"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Bang, "!"),
            (TokenKind::Minus, "-"),
            (TokenKind::Slash, "/"),
            (TokenKind::Asterisk, "*"),
            (TokenKind::Int, "5"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Int, "5"),
            (TokenKind::Lt, "<"),
            (TokenKind::Int, "10"),
            (TokenKind::Gt, ">"),
            (TokenKind::Int, "5"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::If, "if"),
            (TokenKind::LParen, "("),
            (TokenKind::Int, "5"),
            (TokenKind::Lt, "<"),
            (TokenKind::Int, "10"),
            (TokenKind::RParen, ")"),
            (TokenKind::LBrace, "{"),
            (TokenKind::Return, "return"),
            (TokenKind::True, "true"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::RBrace, "}"),
            (TokenKind::Else, "else"),
            (TokenKind::LBrace, "{"),
            (TokenKind::Return, "return"),
            (TokenKind::False, "false"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::RBrace, "}"),
            (TokenKind::Int, "10"),
            (TokenKind::Eq, "=="),
            (TokenKind::Int, "10"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Int, "10"),
            (TokenKind::NotEq, "!="),
            (TokenKind::Int, "9"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::String, "foobar"),
            (TokenKind::String, "foo bar"),
            (TokenKind::LBracket, "["),
            (TokenKind::Int, "1"),
            (TokenKind::Comma, ","),
            (TokenKind::Int, "2"),
            (TokenKind::RBracket, "]"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::LBrace, "{"),
            (TokenKind::String, "foo"),
            (TokenKind::Colon, ":"),
            (TokenKind::String, "bar"),
            (TokenKind::RBrace, "}"),
            (TokenKind::Eof, ""),
        ];

        let mut lexer = Lexer::new(input);

        for (exp_ident, exp_literal) in tests {
            let token = lexer.next_token();
            assert_eq!(token.kind, exp_ident);
            assert_eq!(token.literal, exp_literal);
        }
    }
}
