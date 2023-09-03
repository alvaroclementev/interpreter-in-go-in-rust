//! Contains the main `eval` function

#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::{Expression, Program, Statement};
use crate::object::{Function, Object};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            store: HashMap::new(),
            outer: None,
        }
    }

    fn with_outer(outer: Rc<RefCell<Environment>>) -> Self {
        let mut env = Environment::new();
        env.outer = Some(outer);
        env
    }

    fn get(&self, name: &str) -> Option<Object> {
        // FIXME(alvaro): This should return a reference, however I could not
        // tame the borrow checker to allow me to get references to outer
        // outer environments... so we are cloning
        self.store.get(name).cloned().or_else(|| {
            self.outer
                .as_ref()
                .and_then(|outer| outer.borrow().get(name))
        })
    }

    fn set(&mut self, name: String, value: Object) {
        self.store.insert(name, value);
    }
}

/// This is the trait that must be implemented by the AST nodes
pub trait Eval {
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> Object;
}

pub fn eval(node: &impl Eval, environment: Rc<RefCell<Environment>>) -> Object {
    node.eval(environment)
}

pub fn eval_in_new_env(node: &impl Eval) -> Object {
    let environment = Rc::new(RefCell::new(Environment::new()));
    eval(node, environment)
}

impl Eval for Statement {
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> Object {
        match self {
            Statement::Expression(expr) => expr.eval(environment),
            Statement::Block(block) => {
                let mut result = Object::Null;

                for stmt in block.statements.iter() {
                    match stmt.eval(environment.clone()) {
                        // If the block ended due to a return value, we do not
                        // unwrap it and send it up the stack to signal to nested
                        // blocks that they should return as well
                        retval @ Object::Return(..) => return retval,
                        err @ Object::Error(..) => return err,
                        new_result => result = new_result,
                    }
                }
                result
            }
            Statement::Return(expr) => {
                let value = expr.value.eval(environment);
                if value.is_error() {
                    value
                } else {
                    Object::Return(Rc::new(value))
                }
            }
            Statement::Let(expr) => {
                let value = expr.value.eval(environment.clone());
                if value.is_error() {
                    return value;
                }

                environment.borrow_mut().set(expr.name.value.clone(), value);
                Object::Null
            }
        }
    }
}

impl Eval for Expression {
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> Object {
        match self {
            Expression::IntegerLiteral(lit) => Object::Integer(lit.value),
            Expression::BooleanLiteral(lit) => Object::Boolean(lit.value),
            Expression::StringLiteral(lit) => Object::String(lit.value.clone()),
            Expression::Prefix(expr) => {
                let right = expr.right.eval(environment);
                if right.is_error() {
                    return right;
                }
                match expr.operator.as_ref() {
                    "!" => match right.as_boolean() {
                        Object::Boolean(val) => Object::Boolean(!val),
                        _ => unreachable!(),
                    },
                    "-" => match right {
                        Object::Integer(val) => Object::Integer(-val),
                        val => Object::Error(format!("unknown operator: -{}", val.type_str())),
                    },
                    op => Object::Error(format!("unknown operator: {}{}", op, right.type_str())),
                }
            }
            Expression::Infix(expr) => {
                let left = expr.left.eval(environment.clone());
                if left.is_error() {
                    return left;
                }
                let right = expr.right.eval(environment.clone());
                if right.is_error() {
                    return right;
                }

                match (left, right) {
                    (Object::Integer(left), Object::Integer(right)) => {
                        eval_integer_infix_expression(environment, &expr.operator, left, right)
                    }
                    (Object::String(left), Object::String(right)) => {
                        eval_string_infix_expression(environment, &expr.operator, left, right)
                    }
                    (left, right)
                        if std::mem::discriminant(&left) == std::mem::discriminant(&right) =>
                    {
                        match expr.operator.as_ref() {
                            "==" => Object::Boolean(left == right),
                            "!=" => Object::Boolean(left != right),
                            op => Object::Error(format!(
                                "unknown operator: {} {} {}",
                                left.type_str(),
                                op,
                                right.type_str()
                            )),
                        }
                    }
                    // They are of different types
                    (left, right) => Object::Error(format!(
                        "type mismatch: {} {} {}",
                        left.type_str(),
                        &expr.operator,
                        right.type_str()
                    )),
                }
            }
            Expression::If(expr) => {
                let condition = expr.condition.eval(environment.clone());
                if condition.is_error() {
                    condition
                } else if condition.is_truthy() {
                    expr.consequence.eval(environment.clone())
                } else if let Some(alternative) = &expr.alternative {
                    alternative.eval(environment)
                } else {
                    Object::Null
                }
            }
            Expression::Identifier(expr) => {
                let name = &expr.value;
                environment
                    .borrow()
                    .get(name)
                    .unwrap_or_else(|| Object::Error(format!("identifier not found: {}", name)))
            }
            // FIXME(alvaro): Too much cloning in here...
            Expression::Function(expr) => Object::Function(Box::new(Function::new(
                expr.parameters.clone(),
                expr.body.clone(),
                environment.clone(),
            ))),
            Expression::Call(expr) => {
                let function = expr.function.eval(environment.clone());
                if function.is_error() {
                    return function;
                }
                // TODO(alvaro): If we used a `Result` instead this would not
                // be so awkward
                let mut args = eval_expressions(&expr.arguments, environment);
                if args.len() == 1 && args[0].is_error() {
                    return args.pop().unwrap();
                }
                eval_function_application(function, args)
            }
            expr => todo!("{:?}", expr),
        }
    }
}

impl Eval for Program {
    fn eval(&self, environment: Rc<RefCell<Environment>>) -> Object {
        let mut result = Object::Null;

        for stmt in self.statements.iter() {
            match stmt.eval(environment.clone()) {
                Object::Return(value) => return (*value).clone(),
                err @ Object::Error(..) => return err,
                obj => result = obj,
            }
        }

        result
    }
}

fn eval_integer_infix_expression(
    _environment: Rc<RefCell<Environment>>,
    operator: &str,
    left: i64,
    right: i64,
) -> Object {
    let int_type_str: &str = Object::Integer(0).type_str();
    match operator {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        op => Object::Error(format!(
            "unknown operator: {} {} {}",
            int_type_str, op, int_type_str
        )),
    }
}

fn eval_string_infix_expression(
    _environment: Rc<RefCell<Environment>>,
    operator: &str,
    left: String,
    right: String,
) -> Object {
    match operator {
        "+" => Object::String(format!("{}{}", left, right)),
        op => Object::Error(format!("unknown operator: STRING {} STRING", op)),
    }
}

fn eval_expressions(
    expressions: &[Expression],
    environment: Rc<RefCell<Environment>>,
) -> Vec<Object> {
    let mut result = Vec::new();

    for expr in expressions {
        match expr.eval(environment.clone()) {
            err @ Object::Error(..) => return vec![err],
            new_result => result.push(new_result),
        }
    }
    result
}

fn eval_function_application(fun: Object, args: Vec<Object>) -> Object {
    let Object::Function(fun) = fun else {
        return Object::Error(format!("not a function: {}", fun.type_str()));
    };

    let extended_env = Rc::new(RefCell::new(Environment::with_outer(
        fun.environment.clone(),
    )));

    // Extend the environment
    args.into_iter()
        .zip(fun.parameters.iter())
        .for_each(|(arg, param)| extended_env.borrow_mut().set(param.value.clone(), arg));

    // FIXME(alvaro): It's awkward that we have to wrap this to eval it...
    let body_stmt = Statement::Block(fun.body);
    let result = body_stmt.eval(extended_env);

    // Unwrap the result
    match result {
        // FIXME(alvaro): Unfortunate clone
        Object::Return(value) => (*value).clone(),
        obj => obj,
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
        eval_in_new_env(&program)
    }

    fn check_integer_object(obj: &Object, expected: i64, input: &str) {
        match obj {
            Object::Integer(val) => assert_eq!(*val, expected, "{}", input),
            _ => panic!("expected Integer object, found {:?} ({})", obj, input),
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

    #[test]
    fn test_error_handling() {
        struct Test {
            input: String,
            message: String,
        }

        let tests = [
            Test {
                input: "5 + true;".to_string(),
                message: "type mismatch: INTEGER + BOOLEAN".to_string(),
            },
            Test {
                input: "5 + true; 5;".to_string(),
                message: "type mismatch: INTEGER + BOOLEAN".to_string(),
            },
            Test {
                input: "-true".to_string(),
                message: "unknown operator: -BOOLEAN".to_string(),
            },
            Test {
                input: "true + false".to_string(),
                message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            },
            Test {
                input: "5; true + false; 5".to_string(),
                message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            },
            Test {
                input: "if (10 > 1) { true + false; }".to_string(),
                message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            },
            Test {
                input: "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }".to_string(),
                message: "unknown operator: BOOLEAN + BOOLEAN".to_string(),
            },
            Test {
                input: "foobar".to_string(),
                message: "identifier not found: foobar".to_string(),
            },
            Test {
                input: r#""Hello" - "World""#.to_string(),
                message: "unknown operator: STRING - STRING".to_string(),
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);

            match evaluated {
                Object::Error(msg) => {
                    assert_eq!(&msg, &test.message, "{}", &test.input);
                }
                obj => panic!("expected Error, found: {:?}", obj),
            }
        }
    }

    #[test]
    fn test_let_statement() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = [
            Test {
                input: "let a = 5; a;".to_string(),
                expected: 5,
            },
            Test {
                input: "let a = 5 * 5; a;".to_string(),
                expected: 25,
            },
            Test {
                input: "let a = 5; let b = a; b;".to_string(),
                expected: 5,
            },
            Test {
                input: "let a = 5; let b = a; let c = a + b + 5; c;".to_string(),
                expected: 15,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);

            check_integer_object(&evaluated, test.expected, &test.input);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }".to_string();
        let evaluated = eval_for_test(&input);
        match evaluated {
            Object::Function(fun) => {
                assert_eq!(fun.parameters.len(), 1);
                assert_eq!(fun.parameters[0].to_string(), "x");

                assert_eq!(fun.body.to_string(), "(x + 2)");
            }
            obj => panic!("unexpected value, expected Function but got: {:?}", obj),
        }
    }

    #[test]
    fn test_function_application() {
        struct Test {
            input: String,
            expected: i64,
        }

        let tests = [
            Test {
                input: "let identity = fn(x) { x; }; identity(5);".to_string(),
                expected: 5,
            },
            Test {
                input: "let identity = fn(x) { return x; }; identity(5);".to_string(),
                expected: 5,
            },
            Test {
                input: "let double = fn(x) { x * 2; }; double(5);".to_string(),
                expected: 10,
            },
            Test {
                input: "let add = fn(x, y) { x + y; }; add(5, 5);".to_string(),
                expected: 10,
            },
            Test {
                input: "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));".to_string(),
                expected: 20,
            },
            Test {
                input: "fn(x) { x; }(5)".to_string(),
                expected: 5,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_integer_object(&evaluated, test.expected, &test.input);
        }
    }

    #[test]
    fn test_string_literal() {
        let input = "\"Hello World!\"".to_string();
        let evaluated = eval_for_test(&input);
        match evaluated {
            Object::String(lit) => {
                assert_eq!(lit, "Hello World!");
            }
            obj => panic!("unexpected value, expected String but got: {:?}", obj),
        }
    }

    #[test]
    fn test_string_concatenation() {
        let input = r#""Hello" + " " + "World!""#.to_string();
        let evaluated = eval_for_test(&input);
        match evaluated {
            Object::String(lit) => {
                assert_eq!(lit, "Hello World!");
            }
            obj => panic!("unexpected value, expected String but got: {:?}", obj),
        }
    }
}
