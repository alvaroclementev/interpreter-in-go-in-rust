#![allow(dead_code)]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Illegal,
    Eof,
    // Identifier + Literals
    Identifier,
    Int,
    String,
    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,
    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

impl Token {
    pub fn new(kind: TokenKind, literal: String) -> Self {
        Self { kind, literal }
    }
}
