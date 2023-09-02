//! Contains the main `eval` function

use std::rc::Rc;

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
            Statement::Block(block) => {
                let mut result = Object::Null;

                for stmt in block.statements.iter() {
                    match stmt.eval() {
                        // If the block ended due to a return value, we do not
                        // unwrap it and send it up the stack to signal to nested
                        // blocks that they should return as well
                        retval @ Object::Return(..) => return retval,
                        new_result => result = new_result,
                    }
                }
                result
            }
            Statement::Return(expr) => Object::Return(Rc::new(expr.value.eval())),
            stmt => todo!("{:?}", stmt),
        }
    }
}

impl Eval for Expression {
    fn eval(&self) -> Object {
        match self {
            Expression::IntegerLiteral(lit) => Object::Integer(lit.value),
            Expression::BooleanLiteral(lit) => Object::Boolean(lit.value),
            Expression::Prefix(expr) => {
                let right = expr.right.eval();
                match expr.operator.as_ref() {
                    "!" => match right.as_boolean() {
                        Object::Boolean(val) => Object::Boolean(!val),
                        _ => unreachable!(),
                    },
                    "-" => match right {
                        // TODO(alvaro): Error handling for invalid operations
                        Object::Integer(val) => Object::Integer(-val),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }
            }
            Expression::Infix(expr) => {
                let left = expr.left.eval();
                let right = expr.right.eval();

                match (left, right) {
                    (Object::Integer(left), Object::Integer(right)) => {
                        eval_integer_infix_expression(&expr.operator, left, right)
                    }
                    (left, right) => match expr.operator.as_ref() {
                        "==" => Object::Boolean(left == right),
                        "!=" => Object::Boolean(left != right),
                        op => todo!("other infix operators: {}", op),
                    },
                }
            }
            Expression::If(expr) => {
                let condition = expr.condition.eval();
                if condition.is_truthy() {
                    expr.consequence.eval()
                } else if let Some(alternative) = &expr.alternative {
                    alternative.eval()
                } else {
                    Object::Null
                }
            }
            expr => todo!("{:?}", expr),
        }
    }
}

impl Eval for Program {
    fn eval(&self) -> Object {
        let mut result = Object::Null;

        for stmt in self.statements.iter() {
            match stmt.eval() {
                Object::Return(value) => return (*value).clone(),
                obj => result = obj,
            }
        }

        result
    }
}

fn eval_integer_infix_expression(operator: &str, left: i64, right: i64) -> Object {
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => unreachable!(),
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

    fn check_integer_object(obj: &Object, expected: i64, input: &str) {
        match obj {
            Object::Integer(val) => assert_eq!(*val, expected, "{}", input),
            _ => panic!("expected Integer object, found {:?}", obj),
        };
    }

    fn check_boolean_object(obj: &Object, expected: bool, input: &str) {
        match obj {
            Object::Boolean(val) => assert_eq!(*val, expected, "{}", input),
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
            Test {
                input: "-5".to_string(),
                expected: -5,
            },
            Test {
                input: "-10".to_string(),
                expected: -10,
            },
            Test {
                input: "5 + 5 + 5 + 5 - 10".to_string(),
                expected: 10,
            },
            Test {
                input: "2 * 2 * 2 * 2 * 2".to_string(),
                expected: 32,
            },
            Test {
                input: "-50 + 100 + -50".to_string(),
                expected: 0,
            },
            Test {
                input: "5 * 2 + 10".to_string(),
                expected: 20,
            },
            Test {
                input: "5 + 2 * 20".to_string(),
                expected: 45,
            },
            Test {
                input: "20 + 2 * -10".to_string(),
                expected: 0,
            },
            Test {
                input: "50 / 2 * 2 + 10".to_string(),
                expected: 60,
            },
            Test {
                input: "2 * (5 + 10)".to_string(),
                expected: 30,
            },
            Test {
                input: "3 * 3 * 3 + 10".to_string(),
                expected: 37,
            },
            Test {
                input: "3 * (3 * 3) + 10".to_string(),
                expected: 37,
            },
            Test {
                input: "(5 + 10 * 2 + 15 / 3) * 2 + -10".to_string(),
                expected: 50,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_integer_object(&evaluated, test.expected, &test.input);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        struct Test {
            input: String,
            expected: bool,
        }

        let tests = [
            Test {
                input: "true".to_string(),
                expected: true,
            },
            Test {
                input: "false".to_string(),
                expected: false,
            },
            Test {
                input: "1 < 2".to_string(),
                expected: true,
            },
            Test {
                input: "1 > 2".to_string(),
                expected: false,
            },
            Test {
                input: "1 < 1".to_string(),
                expected: false,
            },
            Test {
                input: "1 > 1".to_string(),
                expected: false,
            },
            Test {
                input: "1 == 1".to_string(),
                expected: true,
            },
            Test {
                input: "1 != 1".to_string(),
                expected: false,
            },
            Test {
                input: "1 == 2".to_string(),
                expected: false,
            },
            Test {
                input: "1 != 2".to_string(),
                expected: true,
            },
            Test {
                input: "true == true".to_string(),
                expected: true,
            },
            Test {
                input: "false == false".to_string(),
                expected: true,
            },
            Test {
                input: "true == false".to_string(),
                expected: false,
            },
            Test {
                input: "true != false".to_string(),
                expected: true,
            },
            Test {
                input: "false != true".to_string(),
                expected: true,
            },
            Test {
                input: "(1 < 2) == true".to_string(),
                expected: true,
            },
            Test {
                input: "(1 < 2) == false".to_string(),
                expected: false,
            },
            Test {
                input: "(1 > 2) == true".to_string(),
                expected: false,
            },
            Test {
                input: "(1 > 2) == false".to_string(),
                expected: true,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_boolean_object(&evaluated, test.expected, &test.input);
        }
    }

    #[test]
    fn test_bang_operator() {
        struct Test {
            input: String,
            expected: bool,
        }

        let tests = [
            Test {
                input: "!true".to_string(),
                expected: false,
            },
            Test {
                input: "!false".to_string(),
                expected: true,
            },
            Test {
                input: "!5".to_string(),
                expected: false,
            },
            Test {
                input: "!!true".to_string(),
                expected: true,
            },
            Test {
                input: "!!false".to_string(),
                expected: false,
            },
            Test {
                input: "!!5".to_string(),
                expected: true,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_boolean_object(&evaluated, test.expected, &test.input);
        }
    }

    #[test]
    fn test_if_else_expresssions() {
        struct Test {
            input: String,
            expected: Object,
        }

        let tests = [
            Test {
                input: "if (true) { 10 }".to_string(),
                expected: Object::Integer(10),
            },
            Test {
                input: "if (false) { 10 }".to_string(),
                expected: Object::Null,
            },
            Test {
                input: "if (1) { 10 }".to_string(),
                expected: Object::Integer(10),
            },
            Test {
                input: "if (1 < 2) { 10 }".to_string(),
                expected: Object::Integer(10),
            },
            Test {
                input: "if (1 > 2) { 10 }".to_string(),
                expected: Object::Null,
            },
            Test {
                input: "if (1 > 2) { 10 } else { 20 }".to_string(),
                expected: Object::Integer(20),
            },
            Test {
                input: "if (1 < 2) { 10 } else { 20 }".to_string(),
                expected: Object::Integer(10),
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            assert_eq!(&evaluated, &test.expected, "{}", &test.input);
        }
    }

    #[test]
    fn test_return_statement() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = [
            Test {
                input: "return 10;".to_string(),
                expected: 10,
            },
            Test {
                input: "return 10; 9;".to_string(),
                expected: 10,
            },
            Test {
                input: "return 2 * 5; 9;".to_string(),
                expected: 10,
            },
            Test {
                input: "9; return 2 * 5; 9;".to_string(),
                expected: 10,
            },
            Test {
                input: "if (10 > 1) { if (10 > 1) { return 10; } return 1; }".to_string(),
                expected: 10,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_integer_object(&evaluated, test.expected, &test.input);
        }
    }
}
