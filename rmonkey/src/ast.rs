#![allow(dead_code)]

use std::{fmt::Display, rc::Rc};

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
}

#[derive(Debug, Clone)]
pub enum Expression {
    Missing,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    Prefix(PrefixExpression),
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Missing => "<missing>",
            Expression::Identifier(expr) => expr.token.literal.as_ref(),
            Expression::IntegerLiteral(expr) => expr.token.literal.as_ref(),
            Expression::Prefix(expr) => expr.token.literal.as_ref(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
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

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl IntegerLiteral {
    pub fn new(token: Token, value: i64) -> Self {
        Self { token, value }
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Rc<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: String, right: Rc<Expression>) -> Self {
        Self {
            token,
            operator,
            right,
        }
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator, self.right)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(Let),
    Return(Return),
    Expression(Expression),
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(stmt) => stmt.token_literal(),
            Statement::Return(stmt) => stmt.token_literal(),
            Statement::Expression(stmt) => stmt.token_literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(stmt) => stmt.fmt(f),
            Statement::Return(stmt) => stmt.fmt(f),
            Statement::Expression(stmt) => stmt.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
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

impl Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name,
            self.value
        )?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Return {
    pub token: Token,
    pub value: Expression,
}

impl Return {
    pub fn new(token: Token, value: Expression) -> Self {
        Self { token, value }
    }
}

impl Node for Return {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {};", self.token_literal(), self.value,)?;
        Ok(())
    }
}

#[derive(Debug)]
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

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.statements
            .iter()
            .try_for_each(|s| write!(f, "{}", s))?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{Token, TokenKind};

    use super::*;

    #[test]
    fn test_string() {
        let program = Program::new(vec![Statement::Let(Let::new(
            Token::new(TokenKind::Let, "let".to_string()),
            Identifier::new(
                Token::new(TokenKind::Identifier, "myVar".to_string()),
                "myVar".to_string(),
            ),
            Expression::Identifier(Identifier::new(
                Token::new(TokenKind::Identifier, "anotherVar".to_string()),
                "anotherVar".to_string(),
            )),
        ))]);

        assert_eq!(format!("{}", program), "let myVar = anotherVar;");
    }
}
