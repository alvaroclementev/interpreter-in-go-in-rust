#![allow(dead_code)]

use itertools::Itertools;
use std::{fmt::Display, rc::Rc};

use crate::token::Token;

pub trait Node {
    fn token_literal(&self) -> &str;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Missing,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    StringLiteral(StringLiteral),
    ArrayLiteral(ArrayLiteral),
    HashLiteral(HashLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    Index(IndexExpression),
}

impl Node for Expression {
    fn token_literal(&self) -> &str {
        match self {
            Expression::Missing => "<missing>",
            Expression::Identifier(expr) => expr.token.literal.as_ref(),
            Expression::IntegerLiteral(expr) => expr.token.literal.as_ref(),
            Expression::BooleanLiteral(expr) => expr.token.literal.as_ref(),
            Expression::StringLiteral(expr) => expr.token.literal.as_ref(),
            Expression::ArrayLiteral(expr) => expr.token.literal.as_ref(),
            Expression::HashLiteral(expr) => expr.token.literal.as_ref(),
            Expression::Prefix(expr) => expr.token.literal.as_ref(),
            Expression::Infix(expr) => expr.token.literal.as_ref(),
            Expression::If(expr) => expr.token.literal.as_ref(),
            Expression::Function(expr) => expr.token.literal.as_ref(),
            Expression::Call(expr) => expr.token.literal.as_ref(),
            Expression::Index(expr) => expr.token.literal.as_ref(),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Missing => write!(f, "{}", self.token_literal()),
            Expression::Identifier(expr) => expr.fmt(f),
            Expression::IntegerLiteral(expr) => expr.fmt(f),
            Expression::BooleanLiteral(expr) => expr.fmt(f),
            Expression::StringLiteral(expr) => expr.fmt(f),
            Expression::ArrayLiteral(expr) => expr.fmt(f),
            Expression::HashLiteral(expr) => expr.fmt(f),
            Expression::Prefix(expr) => expr.fmt(f),
            Expression::Infix(expr) => expr.fmt(f),
            Expression::If(expr) => expr.fmt(f),
            Expression::Function(expr) => expr.fmt(f),
            Expression::Call(expr) => expr.fmt(f),
            Expression::Index(expr) => expr.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}

impl BooleanLiteral {
    pub fn new(token: Token, value: bool) -> Self {
        Self { token, value }
    }
}

impl Node for BooleanLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for BooleanLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

impl StringLiteral {
    pub fn new(token: Token, value: String) -> Self {
        Self { token, value }
    }
}

impl Node for StringLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token_literal())?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<Expression>,
}

impl ArrayLiteral {
    pub fn new(token: Token, elements: Vec<Expression>) -> Self {
        Self { token, elements }
    }
}

impl Node for ArrayLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements = self.elements.iter().map(ToString::to_string).join(", ");
        write!(f, "[{}]", elements)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashLiteral {
    pub token: Token,
    // NOTE(alvaro): Using some Map here is unnecessary, and there are problems
    // with implementing `Hash` for any kind of Expression (which is not
    // necessary)
    pub pairs: Vec<(Expression, Expression)>,
}

impl HashLiteral {
    pub fn new(token: Token, pairs: Vec<(Expression, Expression)>) -> Self {
        Self { token, pairs }
    }
}

impl Node for HashLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for HashLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let elements = self
            .pairs
            .iter()
            .map(|p| format!("{}: {}", p.0, p.1))
            .join(", ");
        write!(f, "{{{}}}", elements)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub operator: String,
    pub right: Rc<Expression>,
}

impl InfixExpression {
    pub fn new(
        token: Token,
        left: Rc<Expression>,
        operator: String,
        right: Rc<Expression>,
    ) -> Self {
        Self {
            token,
            left,
            operator,
            right,
        }
    }
}

impl Node for InfixExpression {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Rc<Expression>,
    pub consequence: Rc<Statement>,
    pub alternative: Option<Rc<Statement>>,
}

impl IfExpression {
    pub fn new(
        token: Token,
        condition: Rc<Expression>,
        consequence: Rc<Statement>,
        alternative: Option<Rc<Statement>>,
    ) -> Self {
        Self {
            token,
            condition,
            consequence,
            alternative,
        }
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.consequence)?;
        match &self.alternative {
            None => Ok(()),
            Some(stmt) => write!(f, " else {}", stmt),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: Block,
}

impl FunctionLiteral {
    pub fn new(token: Token, parameters: Vec<Identifier>, body: Block) -> Self {
        Self {
            token,
            parameters,
            body,
        }
    }
}

impl Node for FunctionLiteral {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self.parameters.iter().map(|p| format!("{}", p)).join(", ");
        write!(f, "{} ({}) {}", self.token_literal(), args, self.body)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallExpression {
    pub token: Token,
    // Identifier or FunctionLiteral
    pub function: Rc<Expression>,
    pub arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new(token: Token, function: Rc<Expression>, arguments: Vec<Expression>) -> Self {
        Self {
            token,
            function,
            arguments,
        }
    }
}

impl Node for CallExpression {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let args = self.arguments.iter().map(|a| format!("{}", a)).join(", ");
        write!(f, "{}({})", self.function, args)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IndexExpression {
    pub token: Token,
    pub left: Rc<Expression>,
    pub right: Rc<Expression>,
}

impl IndexExpression {
    pub fn new(token: Token, left: Rc<Expression>, right: Rc<Expression>) -> Self {
        Self { token, left, right }
    }
}

impl Node for IndexExpression {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.left, self.right)?;
        Ok(())
    }
}

// ----- Statements -----

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(Let),
    Return(Return),
    Expression(Expression),
    Block(Block),
}

impl Node for Statement {
    fn token_literal(&self) -> &str {
        match self {
            Statement::Let(stmt) => stmt.token_literal(),
            Statement::Return(stmt) => stmt.token_literal(),
            Statement::Expression(stmt) => stmt.token_literal(),
            Statement::Block(stmt) => stmt.token_literal(),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(stmt) => stmt.fmt(f),
            Statement::Return(stmt) => stmt.fmt(f),
            Statement::Expression(stmt) => stmt.fmt(f),
            Statement::Block(stmt) => stmt.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub token: Token,
    pub statements: Vec<Statement>,
}

impl Block {
    pub fn new(token: Token, statements: Vec<Statement>) -> Self {
        Self { token, statements }
    }
}

impl Node for Block {
    fn token_literal(&self) -> &str {
        self.token.literal.as_ref()
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.statements
            .iter()
            .try_for_each(|s| write!(f, "{}", s))?;
        Ok(())
    }
}

// ----- Program -----

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
