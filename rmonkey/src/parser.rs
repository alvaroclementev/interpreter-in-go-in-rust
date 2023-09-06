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
    Index,
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
            TokenKind::LParen => Precedence::Call,
            TokenKind::LBracket => Precedence::Index,
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

        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;

        // Skip optional semicolon
        if self.peek_is(TokenKind::Semicolon) {
            self.next_token();
        }

        Some(ast::Statement::Let(ast::Let::new(
            let_token, identifier, value,
        )))
    }

    fn parse_return_statement(&mut self) -> Option<ast::Statement> {
        let ret_token = self.current.clone();

        self.next_token();

        // TODO(alvaro): Better error reporting
        let return_value = self.parse_expression(Precedence::Lowest)?;

        // Skip optional semicolon
        if self.peek_is(TokenKind::Semicolon) {
            self.next_token()
        }

        Some(ast::Statement::Return(ast::Return::new(
            ret_token,
            return_value,
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
            TokenKind::String => self.parse_string_literal(),
            TokenKind::LBracket => self.parse_array_literal(),
            TokenKind::LBrace => self.parse_hash_literal(),
            TokenKind::LParen => self.parse_grouped_expression(),
            TokenKind::If => self.parse_if_expression(),
            TokenKind::Function => self.parse_function_literal(),

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
            TokenKind::LParen => self.parse_call_expression(left),
            TokenKind::LBracket => self.parse_index_expression(left),
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

    fn parse_string_literal(&self) -> Result<ast::Expression, String> {
        let literal = ast::StringLiteral::new(self.current.clone(), self.current.literal.clone());
        Ok(ast::Expression::StringLiteral(literal))
    }

    fn parse_array_literal(&mut self) -> Result<ast::Expression, String> {
        let token = self.current.clone();

        // Parse the elements
        let elements = self.parse_expression_list(TokenKind::RBracket)?;
        let literal = ast::ArrayLiteral::new(token, elements);
        Ok(ast::Expression::ArrayLiteral(literal))
    }

    fn parse_hash_literal(&mut self) -> Result<ast::Expression, String> {
        let token = self.current.clone();
        let mut pairs = Vec::new();

        // Parse the elements
        while !self.peek_is(TokenKind::RBrace) {
            self.next_token();
            let key = self
                .parse_expression(Precedence::Lowest)
                .ok_or("failed to parse key expression in hash literal".to_string())?;
            if !self.expect_peek(TokenKind::Colon) {
                return Err("expected a ':' when parsing hash literal pair".to_string());
            }
            self.next_token();
            let value = self
                .parse_expression(Precedence::Lowest)
                .ok_or("failed to parse value expression in hash literal".to_string())?;

            pairs.push((key, value));

            if !self.peek_is(TokenKind::RBrace) && !self.expect_peek(TokenKind::Comma) {
                return Err("expected '}' or ',' after parsing hash literal pair".to_string());
            }
        }

        if !self.expect_peek(TokenKind::RBrace) {
            return Err("expected '}' after parsing hash literal".to_string());
        }
        let literal = ast::HashLiteral::new(token, pairs);
        Ok(ast::Expression::HashLiteral(literal))
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

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression, String> {
        self.next_token();
        let expr = self
            .parse_expression(Precedence::Lowest)
            .ok_or("failed to parse grouped expression")?;
        if !self.expect_peek(TokenKind::RParen) {
            return Err("expected a ')' when parsing a grouped expression".to_string());
        }
        Ok(expr)
    }

    fn parse_if_expression(&mut self) -> Result<ast::Expression, String> {
        let token = self.current.clone();

        if !self.expect_peek(TokenKind::LParen) {
            return Err("expected a '(' when parsing an if expression".to_string());
        }

        self.next_token();
        let condition = self
            .parse_expression(Precedence::Lowest)
            .ok_or("failed to parse condition expression")?;

        if !self.expect_peek(TokenKind::RParen) {
            return Err("expected a ')' when parsing an if expression".to_string());
        }

        if !self.expect_peek(TokenKind::LBrace) {
            return Err("expected a '{' when parsing an if expression".to_string());
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_is(TokenKind::Else) {
            // Parse alternative
            self.next_token();
            if !self.expect_peek(TokenKind::LBrace) {
                return Err("expected a '{' when parsing an else block".to_string());
            }
            Some(Rc::new(self.parse_block_statement()))
        } else {
            None
        };

        let if_expr = ast::Expression::If(ast::IfExpression::new(
            token,
            Rc::new(condition),
            Rc::new(consequence),
            alternative,
        ));

        Ok(if_expr)
    }

    fn parse_function_literal(&mut self) -> Result<ast::Expression, String> {
        let token = self.current.clone();

        if !self.expect_peek(TokenKind::LParen) {
            return Err("expected '(' when parsing an function literal".to_string());
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenKind::LBrace) {
            return Err("expected a '{' when parsing function literal".to_string());
        }

        let ast::Statement::Block(body) = self.parse_block_statement() else {
            unreachable!()
        };

        let fn_expr = ast::Expression::Function(ast::FunctionLiteral::new(token, parameters, body));

        Ok(fn_expr)
    }

    fn parse_block_statement(&mut self) -> ast::Statement {
        let token = self.current.clone();
        let mut statements = Vec::new();

        self.next_token();

        while !self.current_is(TokenKind::RBrace) && !self.current_is(TokenKind::Eof) {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                statements.push(stmt);
            }
            self.next_token();
        }

        ast::Statement::Block(ast::Block::new(token, statements))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<ast::Identifier>, String> {
        if self.peek_is(TokenKind::RParen) {
            // There are no arguments
            self.next_token();
            return Ok(Vec::new());
        }

        self.next_token();

        let mut parameters = Vec::new();

        let ident = ast::Identifier::new(self.current.clone(), self.current.literal.clone());
        parameters.push(ident);

        while self.peek_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();

            let ident = ast::Identifier::new(self.current.clone(), self.current.literal.clone());
            parameters.push(ident);
        }

        if !self.expect_peek(TokenKind::RParen) {
            return Err("expected a ')' when parsing function parameters".to_string());
        }
        Ok(parameters)
    }

    fn parse_call_expression(
        &mut self,
        function: ast::Expression,
    ) -> Result<ast::Expression, String> {
        let token = self.current.clone();
        let arguments = self.parse_expression_list(TokenKind::RParen)?;

        Ok(ast::Expression::Call(ast::CallExpression::new(
            token,
            Rc::new(function),
            arguments,
        )))
    }

    fn parse_index_expression(&mut self, left: ast::Expression) -> Result<ast::Expression, String> {
        let token = self.current.clone();
        self.next_token();

        let right = self
            .parse_expression(Precedence::Lowest)
            .ok_or_else(|| "failed to parse RHS of index expression".to_string())?;

        if !self.expect_peek(TokenKind::RBracket) {
            return Err("expected a ']' when parsing index expression".to_string());
        }

        Ok(ast::Expression::Index(ast::IndexExpression::new(
            token,
            Rc::new(left),
            Rc::new(right),
        )))
    }

    fn parse_expression_list(
        &mut self,
        end_kind: TokenKind,
    ) -> Result<Vec<ast::Expression>, String> {
        let mut elements = Vec::new();
        if self.peek_is(end_kind) {
            // There are no arguments
            self.next_token();
            return Ok(elements);
        }

        self.next_token();
        let expr = self
            .parse_expression(Precedence::Lowest)
            .ok_or("failed to parse expression list")?;
        elements.push(expr);

        while self.peek_is(TokenKind::Comma) {
            self.next_token();
            self.next_token();

            let expr = self
                .parse_expression(Precedence::Lowest)
                .ok_or("failed to parse expression list element")?;
            elements.push(expr);
        }

        if !self.expect_peek(end_kind) {
            return Err(format!(
                "expected a '{:?}' when parsing expression list element",
                end_kind
            ));
        }
        Ok(elements)
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
            let Statement::Let(let_stmt) = stmt else {
                unreachable!()
            };
            check_literal_expression(&let_stmt.value, test.value);
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
                    check_literal_expression(&ret_stmt.value, test.value);
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
    fn test_string_literal_expression() {
        let input = "\"hello world\"".to_string();

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);
        let literal = &program.statements[0];
        assert!(matches!(
            literal,
            Statement::Expression(Expression::StringLiteral(..))
        ));
        let Statement::Expression(Expression::StringLiteral(literal)) = literal else {
            unreachable!()
        };
        assert_eq!(&literal.value, "hello world");
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
            Test {
                input: "1 + (2 + 3) + 4".to_string(),
                expected: "((1 + (2 + 3)) + 4)".to_string(),
            },
            Test {
                input: "(5 + 5) * 2".to_string(),
                expected: "((5 + 5) * 2)".to_string(),
            },
            Test {
                input: "2 / (5 + 5)".to_string(),
                expected: "(2 / (5 + 5))".to_string(),
            },
            Test {
                input: "-(5 + 5)".to_string(),
                expected: "(-(5 + 5))".to_string(),
            },
            Test {
                input: "!(true == true)".to_string(),
                expected: "(!(true == true))".to_string(),
            },
            Test {
                input: "a + add(b * c) + d".to_string(),
                expected: "((a + add((b * c))) + d)".to_string(),
            },
            Test {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_string(),
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string(),
            },
            Test {
                input: "add(a + b + c * d / f + g)".to_string(),
                expected: "add((((a + b) + ((c * d) / f)) + g))".to_string(),
            },
            Test {
                input: "a * [1, 2, 3, 4][b * c] * d".to_string(),
                expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)".to_string(),
            },
            Test {
                input: "add(a * b[2], b[1], 2 * [1, 2][1])".to_string(),
                expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))".to_string(),
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input.clone());
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            let actual = format!("{}", program);
            assert_eq!(actual, test.expected);
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        assert!(matches!(stmt, Statement::Expression(..)));

        let Statement::Expression(Expression::If(if_expr)) = stmt else {
            unreachable!()
        };

        check_infix_expression(
            &if_expr.condition,
            TestValue::String("x".to_string()),
            "<",
            TestValue::String("y".to_string()),
        );

        // Check the consequence
        let Statement::Block(cons_block) = &*if_expr.consequence else {
            unreachable!()
        };

        assert_eq!(cons_block.statements.len(), 1);
        let consequence = &cons_block.statements[0];
        let Statement::Expression(expr) = consequence else {
            unreachable!()
        };
        check_identifier(expr, "x");

        // Check the alternative
        assert!(if_expr.alternative.is_none());
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        assert!(matches!(stmt, Statement::Expression(..)));

        let Statement::Expression(Expression::If(if_expr)) = stmt else {
            unreachable!()
        };

        check_infix_expression(
            &if_expr.condition,
            TestValue::String("x".to_string()),
            "<",
            TestValue::String("y".to_string()),
        );

        // Check the consequence
        let Statement::Block(cons_block) = &*if_expr.consequence else {
            unreachable!()
        };

        assert_eq!(cons_block.statements.len(), 1);
        let consequence = &cons_block.statements[0];
        let Statement::Expression(expr) = consequence else {
            unreachable!()
        };
        check_identifier(expr, "x");

        // Check the alternative
        match &if_expr.alternative {
            None => panic!("no alternative found"),
            Some(block) => match &**block {
                Statement::Block(alt_block) => {
                    assert_eq!(alt_block.statements.len(), 1);
                    let alternative = &alt_block.statements[0];
                    let Statement::Expression(expr) = alternative else {
                        unreachable!()
                    };
                    check_identifier(expr, "y");
                }
                _ => unreachable!(),
            },
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "fn(x, y) { x + y; }".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        assert!(matches!(
            stmt,
            Statement::Expression(Expression::Function(..))
        ));

        let Statement::Expression(Expression::Function(fn_lit)) = stmt else {
            unreachable!()
        };

        assert_eq!(fn_lit.parameters.len(), 2);

        check_literal_expression(
            &Expression::Identifier(fn_lit.parameters[0].clone()),
            TestValue::String("x".to_string()),
        );
        check_literal_expression(
            &Expression::Identifier(fn_lit.parameters[1].clone()),
            TestValue::String("y".to_string()),
        );

        assert_eq!(fn_lit.body.statements.len(), 1);
        let Statement::Expression(body_expr) = &fn_lit.body.statements[0] else {
            unreachable!()
        };
        check_infix_expression(
            body_expr,
            TestValue::String("x".to_string()),
            "+",
            TestValue::String("y".to_string()),
        );
    }

    #[test]
    fn test_function_parameter_parsing() {
        struct Test {
            input: String,
            expected: Vec<String>,
        }
        let tests = vec![
            Test {
                input: "fn() {};".to_string(),
                expected: vec![],
            },
            Test {
                input: "fn(x) {};".to_string(),
                expected: vec!["x".to_string()],
            },
            Test {
                input: "fn(x, y, z) {};".to_string(),
                expected: vec!["x".to_string(), "y".to_string(), "z".to_string()],
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            assert_eq!(program.statements.len(), 1);

            let Statement::Expression(expr) = &program.statements[0] else {
                unreachable!()
            };

            let Expression::Function(fn_lit) = expr else {
                unreachable!()
            };

            assert_eq!(fn_lit.parameters.len(), test.expected.len());
            for (param, expected) in fn_lit
                .parameters
                .iter()
                .cloned()
                .zip(test.expected.iter().cloned())
            {
                check_literal_expression(
                    &Expression::Identifier(param),
                    TestValue::String(expected),
                );
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5)".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);
        let stmt = &program.statements[0];
        assert!(matches!(stmt, Statement::Expression(Expression::Call(..))));

        let Statement::Expression(Expression::Call(call_expr)) = stmt else {
            unreachable!()
        };

        check_identifier(&call_expr.function, "add");

        assert_eq!(call_expr.arguments.len(), 3);
        check_literal_expression(&call_expr.arguments[0], TestValue::Int(1));
        check_infix_expression(
            &call_expr.arguments[1],
            TestValue::Int(2),
            "*",
            TestValue::Int(3),
        );
        check_infix_expression(
            &call_expr.arguments[2],
            TestValue::Int(4),
            "+",
            TestValue::Int(5),
        );
    }

    #[test]
    fn test_call_expression_parameter_parsing() {
        struct Test {
            input: String,
            expected_ident: String,
            expected_args: Vec<String>,
        }
        let tests = vec![
            Test {
                input: "add()".to_string(),
                expected_ident: "add".to_string(),
                expected_args: vec![],
            },
            Test {
                input: "add(1)".to_string(),
                expected_ident: "add".to_string(),
                expected_args: vec!["1".to_string()],
            },
            Test {
                input: "add(1, 2 * 3, 4 + 5)".to_string(),
                expected_ident: "add".to_string(),
                expected_args: vec![
                    "1".to_string(),
                    "(2 * 3)".to_string(),
                    "(4 + 5)".to_string(),
                ],
            },
        ];

        for test in tests {
            let lexer = Lexer::new(test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            check_parser_errors(&mut parser);

            assert_eq!(program.statements.len(), 1);

            let Statement::Expression(Expression::Call(call)) = &program.statements[0] else {
                unreachable!()
            };

            check_identifier(&call.function, test.expected_ident);
            assert_eq!(call.arguments.len(), test.expected_args.len());

            for (arg, expected) in call.arguments.iter().zip(test.expected_args.iter()) {
                assert_eq!(&format!("{}", arg), expected);
            }
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::ArrayLiteral(array)) = &program.statements[0] else {
            unreachable!()
        };

        assert_eq!(array.elements.len(), 3);
        check_integer_literal(&array.elements[0], 1);
        check_infix_expression(
            &array.elements[1],
            TestValue::Int(2),
            "*",
            TestValue::Int(2),
        );
        check_infix_expression(
            &array.elements[2],
            TestValue::Int(3),
            "+",
            TestValue::Int(3),
        );
    }

    #[test]
    fn test_parsing_index_expressions() {
        let input = "myArray[1 + 1]".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::Index(index)) = &program.statements[0] else {
            unreachable!()
        };

        check_identifier(&index.left, "myArray");
        check_infix_expression(&index.right, TestValue::Int(1), "+", TestValue::Int(1));
    }

    #[test]
    fn test_parsing_hash_literals_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#.to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::HashLiteral(hash)) = &program.statements[0] else {
            unreachable!()
        };

        assert_eq!(hash.pairs.len(), 3);
        let expected = vec![("one", 1), ("two", 2), ("three", 3)];

        for ((key, value), (exp_key, exp_value)) in hash.pairs.iter().zip(expected.into_iter()) {
            match key {
                Expression::StringLiteral(lit) => {
                    assert_eq!(lit.value, exp_key);
                    check_integer_literal(value, exp_value);
                }
                expr => panic!(
                    "invalid expression: expected StringLiteral but got {:?}",
                    expr
                ),
            }
        }
    }

    #[test]
    fn test_parsing_empty_hash_literal() {
        let input = "{}".to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::HashLiteral(hash)) = &program.statements[0] else {
            unreachable!()
        };

        assert_eq!(hash.pairs.len(), 0);
    }

    #[test]
    fn test_parsing_hash_literals_with_expressions() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#.to_string();
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&mut parser);

        assert_eq!(program.statements.len(), 1);

        let Statement::Expression(Expression::HashLiteral(hash)) = &program.statements[0] else {
            unreachable!()
        };

        assert_eq!(hash.pairs.len(), 3);

        struct Test<'a> {
            key: &'a str,
            value: i64,
            operation: (i64, &'a str, i64),
            input: &'a str,
        }

        let expected = vec![Test {
            key: "one",
            value: 1,
            operation: (0, "+", 1),
            input: "0 + 1",
        }];

        for ((key, value), test) in hash.pairs.iter().zip(expected.into_iter()) {
            let Expression::StringLiteral(lit) = key else {
                panic!(
                    "invalid expression: expected StringLiteral but got {:?}",
                    key
                );
            };
            assert_eq!(lit.value, test.key);
            let (left, op, right) = test.operation;
            check_infix_expression(value, TestValue::Int(left), op, TestValue::Int(right));
        }
    }
}
