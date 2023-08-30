#![allow(dead_code)]

use std::rc::Rc;

use crate::{
    ast,
    lexer::Lexer,
    token::{Token, TokenKind},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    lexer: Lexer,

    pub current: Token,
    pub peek: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let current = lexer.next_token();
        let peek = lexer.next_token();
        Self {
            lexer,
            current,
            peek,
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> ast::Program {
        let mut statements = Vec::new();
        loop {
            if self.current_is(TokenKind::Eof) {
                break;
            }

            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }

            self.next_token();
        }

        ast::Program::new(statements)
    }

    fn next_token(&mut self) {
        std::mem::swap(&mut self.current, &mut self.peek);
        self.peek = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<ast::Statement> {
        match self.current.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<ast::Statement> {
        let let_token = self.current.clone();

        // This advances the lexer
        if !self.expect_peek(TokenKind::Identifier) {
            return None;
        }

        let ident_token = self.current.clone();
        let literal = ident_token.literal.clone();
        let identifier = ast::Identifier::new(ident_token, literal);

        if !self.expect_peek(TokenKind::Assign) {
            return None;
        }

        // TODO(alvaro): We should parse the real value here
        loop {
            // Skip until the semicolon
            if self.current_is(TokenKind::Semicolon) {
                break;
            }
            self.next_token()
        }

        Some(ast::Statement::Let(ast::Let::new(
            let_token,
            identifier,
            ast::Expression::Missing,
        )))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let ret_token = self.current.clone();

        self.next_token();

        // TODO(alvaro): We should parse the real value here
        loop {
            // Skip until the semicolon
            if self.current_is(TokenKind::Semicolon) {
                break;
            }
            self.next_token()
        }

        Some(ast::Statement::Return(ast::Return::new(
            ret_token,
            ast::Expression::Missing,
        )))
    }

    fn parse_expression_statement(&mut self) -> Option<ast::Statement> {
        println!("Parsing Expression Statement: {:?}", &self.current);

        let expr = self.parse_expression(Precedence::Lowest)?;
        let statement = ast::Statement::Expression(expr);

        if self.peek_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_expression(&mut self, _precedence: Precedence) -> Option<ast::Expression> {
        self.parse_expression_as_prefix(self.current.kind)
    }

    fn parse_expression_as_prefix(&mut self, kind: TokenKind) -> Option<ast::Expression> {
        let result = match kind {
            TokenKind::Identifier => self.parse_identifier(),
            TokenKind::Int => self.parse_integer_literal(),
            TokenKind::Bang => self.parse_prefix_expression(),
            TokenKind::Minus => self.parse_prefix_expression(),
            kind => Err(format!("unimplemented ({:?})", kind)),
        };
        // Report the error and return
        match result {
            Ok(r) => Some(r),
            Err(msg) => {
                self.errors.push(msg);
                None
            }
        }
    }

    fn parse_identifier(&self) -> Result<ast::Expression, String> {
        let ident = ast::Identifier::new(self.current.clone(), self.current.literal.clone());
        Ok(ast::Expression::Identifier(ident))
    }

    fn parse_integer_literal(&self) -> Result<ast::Expression, String> {
        let value = self
            .current
            .literal
            .parse()
            .map_err(|e| format!("error parsing integer literal: {e}"))?;
        let literal = ast::IntegerLiteral::new(self.current.clone(), value);
        Ok(ast::Expression::IntegerLiteral(literal))
    }

    fn parse_prefix_expression(&mut self) -> Result<ast::Expression, String> {
        let token = self.current.clone();
        let literal = token.literal.clone();

        self.next_token();
        let right_expr = self
            .parse_expression(Precedence::Prefix)
            .ok_or("failed to parse RHS of prefix expression")?;

        let prefix = ast::PrefixExpression::new(token, literal, Rc::new(right_expr));

        Ok(ast::Expression::Prefix(prefix))
    }

    fn current_is(&self, kind: TokenKind) -> bool {
        std::mem::discriminant(&self.current.kind) == std::mem::discriminant(&kind)
    }

    fn peek_is(&self, kind: TokenKind) -> bool {
        std::mem::discriminant(&self.peek.kind) == std::mem::discriminant(&kind)
    }

    /// Assert the kind of the peek token and advance
    fn expect_peek(&mut self, kind: TokenKind) -> bool {
        if self.peek_is(kind) {
            self.next_token();
            true
        } else {
            self.peek_error(kind);
            false
        }
    }

    /// Add an error to the `errors` list due to peek
    fn peek_error(&mut self, kind: TokenKind) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            kind, self.peek.kind
        ));
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Node, Statement};

    use super::*;

    fn check_parser_errors(parser: &mut Parser) {
        if parser.errors.is_empty() {
            return;
        }

        for error in &parser.errors {
            println!("parser error: \"{}\"", error);
        }

        panic!("found parser errors");
    }

    fn check_integer_literal(expr: &Expression, value: i64) {
        match expr {
            Expression::IntegerLiteral(lit) => {
                assert_eq!(lit.value, value);
                assert_eq!(lit.token_literal(), value.to_string());
            }
            _ => panic!("expr is not an IntegerLiteral: {:?}", expr),
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;"
            .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 3);

        let expected_idents = ["x", "y", "foobar"];

        for (statement, exp_ident) in program.statements.into_iter().zip(expected_idents) {
            // Check the type of node
            assert!(matches!(statement, Statement::Let(..)));

            // Check that it contains a "let" value
            assert_eq!(statement.token_literal(), "let");
            let Statement::Let(let_stmt) = statement else {
                unreachable!()
            };

            // Check that the name is the expected identifier value
            assert_eq!(let_stmt.name.value, exp_ident);

            // Check that the token literal is also the expected value
            assert_eq!(let_stmt.name.token_literal(), exp_ident);
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5;
return 10;
return 993322;"
            .to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        check_parser_errors(&mut parser);
        assert_eq!(program.statements.len(), 3);

        let expected_values = ["5", "10", "993322"];

        for (statement, exp_value) in program.statements.into_iter().zip(expected_values) {
            // Check the type of node
            assert!(matches!(statement, Statement::Return(..)));

            // Check that it contains a "return" value
            assert_eq!(statement.token_literal(), "return");
            let Statement::Return(ret_stmt) = statement else {
                unreachable!()
            };

            // Check that the token literal is also the expected value
            // TODO(alvaro): Actually implement this
            // assert_eq!(ret_stmt.value.token_literal(), exp_value);
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);
        let ident = &program.statements[0];
        assert!(matches!(
            ident,
            Statement::Expression(Expression::Identifier(..))
        ));

        let Statement::Expression(Expression::Identifier(ident)) = ident else {
            unreachable!()
        };

        assert_eq!(ident.value, "foobar");
        assert_eq!(ident.token_literal(), "foobar");
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);
        let literal = &program.statements[0];
        assert!(matches!(literal, Statement::Expression(..)));
        let Statement::Expression(literal) = literal else {
            unreachable!()
        };
        check_integer_literal(&literal, 5);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct Test {
            input: String,
            operator: String,
            integer_value: i64,
        }
        let tests = vec![
            Test {
                input: "!5".to_string(),
                operator: "!".to_string(),
                integer_value: 5,
            },
            // Test {
            //     input: "-15".to_string(),
            //     operator: "-".to_string(),
            //     integer_value: 15,
            // },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            assert_eq!(program.statements.len(), 1);
            let expr = &program.statements[0];
            assert!(matches!(
                expr,
                Statement::Expression(Expression::Prefix(..))
            ));

            let Statement::Expression(Expression::Prefix(expr)) = expr else {
                unreachable!()
            };

            assert_eq!(expr.operator, test.operator);
            check_integer_literal(&expr.right, test.integer_value);
        }
    }
}
