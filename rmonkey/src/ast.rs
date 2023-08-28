#![allow(dead_code)]

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
}

pub enum Expression {
    Missing,
    Identifier(Identifier),
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Missing => "<missing>",
            Expression::Identifier(expr) => expr.token.literal.as_ref(),
        }
    }
}

pub enum Statement {
    Let(Let),
}

impl Statement {}

pub struct Let {
    pub token: Token,
    // TODO(alvaro): This should point to the same identifier
    pub name: Identifier,
    pub value: Expression,
}

impl Let {
    pub fn new(token: Token, name: Identifier, value: Expression) -> Self {
        Self { token, name, value }
    }
}

impl Node for Let {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Identifier {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(stmt) => stmt.token_literal(),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self { statements }
    }
}

impl Node for Program {
    fn token_literal(&self) -> &str {
        if self.statements.is_empty() {
            ""
        } else {
            self.statements[0].token_literal()
        }
    }
}
