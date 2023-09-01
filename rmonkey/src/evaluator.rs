//! Contains the main `eval` function

use crate::ast::{Expression, Program, Statement};
use crate::object::Object;

/// This is the trait that must be implemented by the AST nodes
pub trait Eval {
    fn eval(&self) -> Object;
}

pub fn eval(node: &impl Eval) -> Object {
    node.eval()
}

impl Eval for Statement {
    fn eval(&self) -> Object {
        match self {
            Statement::Expression(expr) => expr.eval(),
            stmt => todo!("{}", stmt),
        }
    }
}

impl Eval for Expression {
    fn eval(&self) -> Object {
        match self {
            Expression::IntegerLiteral(lit) => Object::Integer(lit.value),
            expr => todo!("{}", expr),
        }
    }
}

impl Eval for Program {
    fn eval(&self) -> Object {
        let mut result = Object::Null;

        for stmt in self.statements.iter() {
            result = stmt.eval();
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    fn eval_for_test(input: &str) -> Object {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        eval(&program)
    }

    fn check_integer_object(obj: &Object, expected: i64) {
        match obj {
            Object::Integer(val) => assert_eq!(*val, expected),
            _ => panic!("expected Integer object, found {:?}", obj),
        };
    }

    #[test]
    fn test_eval_integer_expression() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = [
            Test {
                input: "5".to_string(),
                expected: 5,
            },
            Test {
                input: "10".to_string(),
                expected: 10,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_integer_object(&evaluated, test.expected);
        }
    }
}
