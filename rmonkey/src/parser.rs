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

impl Precedence {
    fn for_token(token: TokenKind) -> Self {
        match token {
            TokenKind::Eq => Precedence::Equals,
            TokenKind::NotEq => Precedence::Equals,
            TokenKind::Lt => Precedence::LessGreater,
            TokenKind::Gt => Precedence::LessGreater,
            TokenKind::Plus => Precedence::Sum,
            TokenKind::Minus => Precedence::Sum,
            TokenKind::Slash => Precedence::Product,
            TokenKind::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
    }
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
        let expr = self.parse_expression(Precedence::Lowest)?;
        let statement = ast::Statement::Expression(expr);

        if self.peek_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(statement)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<ast::Expression> {
        let mut left = self.parse_expression_as_prefix(self.current.kind)?;

        while !self.peek_is(TokenKind::Semicolon) && precedence < self.peek_precedence() {
            let peek_kind = self.peek.kind;
            self.next_token();
            // NOTE(alvaro): This clone is a bit sad
            if let Some(new_left) = self.parse_expression_as_infix(peek_kind, left.clone()) {
                left = new_left;
            } else {
                break;
            }
        }
        Some(left)
    }

    fn parse_expression_as_prefix(&mut self, kind: TokenKind) -> Option<ast::Expression> {
        let result = match kind {
            TokenKind::Identifier => self.parse_identifier(),
            TokenKind::Int => self.parse_integer_literal(),
            TokenKind::Bang => self.parse_prefix_expression(),
            TokenKind::Minus => self.parse_prefix_expression(),
            TokenKind::True => self.parse_boolean_literal(),
            TokenKind::False => self.parse_boolean_literal(),

            kind => Err(format!("unimplemented prefix ({:?})", kind)),
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

    fn parse_expression_as_infix(
        &mut self,
        kind: TokenKind,
        left: ast::Expression,
    ) -> Option<ast::Expression> {
        let result = match kind {
            TokenKind::Plus => self.parse_infix_expression(left),
            TokenKind::Minus => self.parse_infix_expression(left),
            TokenKind::Slash => self.parse_infix_expression(left),
            TokenKind::Asterisk => self.parse_infix_expression(left),
            TokenKind::Eq => self.parse_infix_expression(left),
            TokenKind::NotEq => self.parse_infix_expression(left),
            TokenKind::Lt => self.parse_infix_expression(left),
            TokenKind::Gt => self.parse_infix_expression(left),
            kind => Err(format!("unimplemented infix ({:?})", kind)),
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

    fn parse_boolean_literal(&self) -> Result<ast::Expression, String> {
        let value = self.current_is(TokenKind::True);
        let literal = ast::BooleanLiteral::new(self.current.clone(), value);
        Ok(ast::Expression::BooleanLiteral(literal))
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

    fn parse_infix_expression(&mut self, left: ast::Expression) -> Result<ast::Expression, String> {
        let token = self.current.clone();
        let literal = token.literal.clone();

        let precedence = self.current_precedence();

        self.next_token();

        let right_expr = self
            .parse_expression(precedence)
            .ok_or("failed to parse RHS of infix expression")?;

        let infix = ast::InfixExpression::new(token, Rc::new(left), literal, Rc::new(right_expr));

        Ok(ast::Expression::Infix(infix))
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

    fn peek_precedence(&self) -> Precedence {
        Precedence::for_token(self.peek.kind)
    }

    fn current_precedence(&self) -> Precedence {
        Precedence::for_token(self.current.kind)
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Node, Statement};

    use super::*;

    enum TestValue {
        Int(i64),
        Bool(bool),
        String(String),
    }

    fn check_parser_errors(parser: &mut Parser) {
        if parser.errors.is_empty() {
            return;
        }

        for error in &parser.errors {
            println!("parser error: \"{}\"", error);
        }

        panic!("found parser errors");
    }

    fn check_let_statement(stmt: &Statement, name: String) {
        match stmt {
            Statement::Let(stmt) => {
                assert_eq!(stmt.token_literal(), "let");
                assert_eq!(stmt.name.value, name);
                assert_eq!(stmt.name.token_literal(), name);
            }
            stmt => panic!("stmt is not a Let: {:?}", stmt),
        }
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

    fn check_bool_literal(expr: &Expression, value: bool) {
        match expr {
            Expression::BooleanLiteral(lit) => {
                assert_eq!(lit.value, value);
                assert_eq!(lit.token_literal(), value.to_string());
            }
            _ => panic!("expr is not an BooleanLiteral: {:?}", expr),
        }
    }

    fn check_identifier(expr: &Expression, value: impl AsRef<str>) {
        match expr {
            Expression::Identifier(ident) => {
                assert_eq!(ident.value, value.as_ref());
                assert_eq!(ident.token_literal(), value.as_ref());
            }
            _ => panic!("expr is not an Identifier: {:?}", expr),
        }
    }

    fn check_literal_expression(expr: &Expression, expected: TestValue) {
        match expected {
            TestValue::Int(value) => check_integer_literal(expr, value),
            TestValue::Bool(value) => check_bool_literal(expr, value),
            TestValue::String(value) => check_identifier(expr, value),
        }
    }

    fn check_infix_expression(
        expr: &Expression,
        left: TestValue,
        operator: &str,
        right: TestValue,
    ) {
        let Expression::Infix(expr) = expr else {
            panic!("expr is not an Infix: {:?}", expr)
        };

        check_literal_expression(&expr.left, left);
        assert_eq!(expr.operator, operator);
        check_literal_expression(&expr.right, right);
    }

    #[test]
    fn test_let_statements() {
        struct Test {
            input: String,
            ident: String,
            value: TestValue,
        }

        let tests = [
            Test {
                input: "let x = 5;".to_string(),
                ident: "x".to_string(),
                value: TestValue::Int(5),
            },
            Test {
                input: "let y = true;".to_string(),
                ident: "y".to_string(),
                value: TestValue::Bool(true),
            },
            Test {
                input: "let foobar = y;".to_string(),
                ident: "foobar".to_string(),
                value: TestValue::String("y".to_string()),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];
            check_let_statement(stmt, test.ident);
            let Statement::Let(_let_stmt) = stmt else {
                unreachable!()
            };
            // FIXME(alvaro): Implement parsing the value of the let expression
            // check_literal_expression(&let_stmt.value, test.value);
        }
    }

    #[test]
    fn test_return_statements() {
        struct Test {
            input: String,
            value: TestValue,
        }

        let tests = [
            Test {
                input: "return 5;".to_string(),
                value: TestValue::Int(5),
            },
            Test {
                input: "return false;".to_string(),
                value: TestValue::Bool(false),
            },
            Test {
                input: "return foobar;".to_string(),
                value: TestValue::String("foobar".to_string()),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];
            match stmt {
                Statement::Return(ret_stmt) => {
                    assert_eq!(ret_stmt.token_literal(), "return");
                    // FIXME(alvaro): Implement parsing the value of the return
                    // check_literal_expression(&ret_stmt.value, test.value);
                }
                stmt => panic!("stmt is not a Return: {:?}", stmt),
            }
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
        check_integer_literal(literal, 5);
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        struct Test {
            input: String,
            operator: String,
            value: TestValue,
        }
        let tests = vec![
            Test {
                input: "!5".to_string(),
                operator: "!".to_string(),
                value: TestValue::Int(5),
            },
            Test {
                input: "-15".to_string(),
                operator: "-".to_string(),
                value: TestValue::Int(15),
            },
            Test {
                input: "!true".to_string(),
                operator: "!".to_string(),
                value: TestValue::Bool(true),
            },
            Test {
                input: "!false".to_string(),
                operator: "!".to_string(),
                value: TestValue::Bool(false),
            },
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
            check_literal_expression(&expr.right, test.value);
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        struct Test {
            input: String,
            left: TestValue,
            operator: String,
            right: TestValue,
        }
        let tests = vec![
            Test {
                input: "5 + 5;".to_string(),
                left: TestValue::Int(5),
                operator: "+".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "5 - 5;".to_string(),
                left: TestValue::Int(5),
                operator: "-".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "5 * 5;".to_string(),
                left: TestValue::Int(5),
                operator: "*".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "5 / 5;".to_string(),
                left: TestValue::Int(5),
                operator: "/".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "5 > 5;".to_string(),
                left: TestValue::Int(5),
                operator: ">".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "5 < 5;".to_string(),
                left: TestValue::Int(5),
                operator: "<".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "5 == 5;".to_string(),
                left: TestValue::Int(5),
                operator: "==".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "5 != 5;".to_string(),
                left: TestValue::Int(5),
                operator: "!=".to_string(),
                right: TestValue::Int(5),
            },
            Test {
                input: "foobar + barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: "+".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "foobar - barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: "-".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "foobar * barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: "*".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "foobar / barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: "/".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "foobar > barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: ">".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "foobar < barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: "<".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "foobar == barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: "==".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "foobar != barfoo;".to_string(),
                left: TestValue::String("foobar".to_string()),
                operator: "!=".to_string(),
                right: TestValue::String("barfoo".to_string()),
            },
            Test {
                input: "true == true".to_string(),
                left: TestValue::Bool(true),
                operator: "==".to_string(),
                right: TestValue::Bool(true),
            },
            Test {
                input: "true != false".to_string(),
                left: TestValue::Bool(true),
                operator: "!=".to_string(),
                right: TestValue::Bool(false),
            },
            Test {
                input: "false == false".to_string(),
                left: TestValue::Bool(false),
                operator: "==".to_string(),
                right: TestValue::Bool(false),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            assert_eq!(program.statements.len(), 1);
            let expr = &program.statements[0];
            let Statement::Expression(expr) = expr else {
                unreachable!()
            };
            check_infix_expression(expr, test.left, &test.operator, test.right);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct Test {
            input: String,
            expected: String,
        }
        let tests = vec![
            Test {
                input: "-a * b".to_string(),
                expected: "((-a) * b)".to_string(),
            },
            Test {
                input: "!-a".to_string(),
                expected: "(!(-a))".to_string(),
            },
            Test {
                input: "a + b + c".to_string(),
                expected: "((a + b) + c)".to_string(),
            },
            Test {
                input: "a + b - c".to_string(),
                expected: "((a + b) - c)".to_string(),
            },
            Test {
                input: "a * b * c".to_string(),
                expected: "((a * b) * c)".to_string(),
            },
            Test {
                input: "a * b / c".to_string(),
                expected: "((a * b) / c)".to_string(),
            },
            Test {
                input: "a + b / c".to_string(),
                expected: "(a + (b / c))".to_string(),
            },
            Test {
                input: "a + b * c + d / e - f".to_string(),
                expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
            },
            Test {
                input: "3 + 4; -5 * 5".to_string(),
                expected: "(3 + 4)((-5) * 5)".to_string(),
            },
            Test {
                input: "5 > 4 == 3 < 4".to_string(),
                expected: "((5 > 4) == (3 < 4))".to_string(),
            },
            Test {
                input: "5 < 4 != 3 > 4".to_string(),
                expected: "((5 < 4) != (3 > 4))".to_string(),
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },
            Test {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },
            Test {
                input: "true".to_string(),
                expected: "true".to_string(),
            },
            Test {
                input: "false".to_string(),
                expected: "false".to_string(),
            },
            Test {
                input: "3 > 5 == false".to_string(),
                expected: "((3 > 5) == false)".to_string(),
            },
            Test {
                input: "3 < 5 == true".to_string(),
                expected: "((3 < 5) == true)".to_string(),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            let actual = format!("{}", program);
            assert_eq!(actual, test.expected);
        }
    }
}
