//! Contains the main `eval` function

#![allow(dead_code)]

mod builtins;

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
            Expression::ArrayLiteral(lit) => {
                let mut elements = eval_expressions(&lit.elements, environment);
                if elements.len() == 1 && elements[0].is_error() {
                    return elements.pop().unwrap();
                }
                Object::Array(elements)
            }
            Expression::HashLiteral(lit) => {
                let mut hash = HashMap::new();

                // Fill out the hashmap
                for (key_expr, val_expr) in lit.pairs.iter() {
                    let key = key_expr.eval(environment.clone());
                    if key.is_error() {
                        return key;
                    }

                    // Check if the key is hashable
                    if !key.is_hashable() {
                        return Object::Error(format!("unusable as hash key: {}", key.type_str()));
                    }

                    let value = val_expr.eval(environment.clone());
                    if value.is_error() {
                        return value;
                    }

                    hash.insert(key, value);
                }

                Object::Hash(hash)
            }
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
                if let Some(ident) = environment.borrow().get(name) {
                    return ident;
                }

                // Check if the value is a builtin function
                if let Some(builtin) = builtins::get_builtin(name) {
                    return Object::Builtin(builtin);
                }

                // The identifier does not exist
                Object::Error(format!("identifier not found: {}", name))
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
                //
                // TODO(alvaro): If we used a `Result` instead this would not
                // be so awkward
                let mut args = eval_expressions(&expr.arguments, environment);
                if args.len() == 1 && args[0].is_error() {
                    return args.pop().unwrap();
                }
                eval_function_application(function, args)
            }
            Expression::Index(expr) => {
                let left = expr.left.eval(environment.clone());
                if left.is_error() {
                    return left;
                }

                let right = expr.right.eval(environment);
                if right.is_error() {
                    return right;
                }

                match left {
                    Object::Array(elements) => match right {
                        Object::Integer(index) => {
                            if index < 0 || index >= elements.len() as i64 {
                                Object::Null
                            } else {
                                elements[index as usize].clone()
                            }
                        }
                        obj => Object::Error(format!("invalid index type: {}", obj.type_str())),
                    },
                    Object::Hash(hash) => {
                        if !right.is_hashable() {
                            Object::Error(format!("unusable as hash key: {}", right.type_str()))
                        } else {
                            hash.get(&right).cloned().unwrap_or(Object::Null)
                        }
                    }
                    obj => {
                        Object::Error(format!("index operator not supported: {}", obj.type_str()))
                    }
                }
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
    match fun {
        Object::Builtin(builtin) => builtin.call(args),
        Object::Function(fun) => {
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
        obj => Object::Error(format!("not a function: {}", obj.type_str())),
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    enum TestValue {
        Null,
        Int(i64),
        Boolean(bool),
        String(String),
        Error(String),
    }

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

    fn check_string_object(obj: &Object, expected: &str, input: &str) {
        match obj {
            Object::String(val) => assert_eq!(val, expected, "{}", input),
            _ => panic!("expected String object, found {:?}", obj),
        };
    }

    fn check_error_object(obj: &Object, expected: &str, input: &str) {
        match obj {
            Object::Error(msg) => assert_eq!(msg, expected, "{}", input),
            obj => panic!("expected Error object, found {:?}", obj),
        };
    }

    fn check_object(evaluated: &Object, expected: &TestValue, input: &str) {
        match expected {
            TestValue::Null => assert!(matches!(evaluated, Object::Null), "{}", input),
            TestValue::Int(value) => check_integer_object(evaluated, *value, input),
            TestValue::Boolean(value) => check_boolean_object(evaluated, *value, input),
            TestValue::String(value) => check_string_object(evaluated, value, input),
            TestValue::Error(message) => check_error_object(evaluated, message, input),
        }
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
            Test {
                input: r#"{"name": "Monkey"}[fn(x) { x }];"#.to_string(),
                message: "unusable as hash key: FUNCTION".to_string(),
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

    #[test]
    fn test_builtin_functions() {
        struct Test {
            input: String,
            expected: TestValue,
        }

        let tests = [
            Test {
                input: "len(\"\")".to_string(),
                expected: TestValue::Int(0),
            },
            Test {
                input: "len(\"four\")".to_string(),
                expected: TestValue::Int(4),
            },
            Test {
                input: "len(\"hello world\")".to_string(),
                expected: TestValue::Int(11),
            },
            Test {
                input: "len(1)".to_string(),
                expected: TestValue::Error(
                    "argument to `len` not supported, got INTEGER".to_string(),
                ),
            },
            Test {
                input: "len(\"one\", \"two\")".to_string(),
                expected: TestValue::Error("wrong number of arguments. got=2, want=1".to_string()),
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_object(&evaluated, &test.expected, &test.input);
        }
    }

    #[test]
    fn test_array_literals() {
        let input = "[1, 2 * 2, 3 + 3]".to_string();
        let evaluated = eval_for_test(&input);
        match evaluated {
            Object::Array(array) => {
                assert_eq!(array.len(), 3);
                check_integer_object(&array[0], 1, "1");
                check_integer_object(&array[1], 4, "2 * 2");
                check_integer_object(&array[2], 6, "3 + 3");
            }
            obj => panic!("unexpected value, expected Array but got: {:?}", obj),
        }
    }

    #[test]
    fn test_array_index_expressions() {
        struct Test {
            input: String,
            expected: TestValue,
        }

        let tests = [
            Test {
                input: "[1, 2, 3][0]".to_string(),
                expected: TestValue::Int(1),
            },
            Test {
                input: "[1, 2, 3][1]".to_string(),
                expected: TestValue::Int(2),
            },
            Test {
                input: "[1, 2, 3][2]".to_string(),
                expected: TestValue::Int(3),
            },
            Test {
                input: "let i = 0; [1][i];".to_string(),
                expected: TestValue::Int(1),
            },
            Test {
                input: "[1, 2, 3][1 + 1]".to_string(),
                expected: TestValue::Int(3),
            },
            Test {
                input: "let myArray = [1, 2, 3]; myArray[2]".to_string(),
                expected: TestValue::Int(3),
            },
            Test {
                input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2]".to_string(),
                expected: TestValue::Int(6),
            },
            Test {
                input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]".to_string(),
                expected: TestValue::Int(2),
            },
            Test {
                input: "[1, 2, 3][3]".to_string(),
                expected: TestValue::Null,
            },
            Test {
                input: "[1, 2, 3][-1]".to_string(),
                expected: TestValue::Null,
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_object(&evaluated, &test.expected, &test.input);
        }
    }

    #[test]
    fn test_hash_literals() {
        let input = r#"let two = "two";
        {
            "one": 10 - 9,
            two: 1 + 1,
            "thr" + "ee": 6 / 2,
            4: 4,
            true: 5,
            false: 6
        }
        "#
        .to_string();
        let evaluated = eval_for_test(&input);

        let expected = vec![
            (Object::String("one".to_string()), Object::Integer(1)),
            (Object::String("two".to_string()), Object::Integer(2)),
            (Object::String("three".to_string()), Object::Integer(3)),
            (Object::Integer(4), Object::Integer(4)),
            (Object::Boolean(true), Object::Integer(5)),
            (Object::Boolean(false), Object::Integer(6)),
        ];

        let Object::Hash(hash) = evaluated else {
            unreachable!()
        };
        for (exp_key, exp_value) in expected {
            assert!(hash.contains_key(&exp_key));
            assert_eq!(hash.get(&exp_key).unwrap(), &exp_value);
        }
    }

    #[test]
    fn test_hash_index_expressions() {
        struct Test {
            input: String,
            expected: TestValue,
        }

        let tests = [
            Test {
                input: r#"{"foo": 5}["foo"]"#.to_string(),
                expected: TestValue::Int(5),
            },
            Test {
                input: r#"{"foo": 5}["bar"]"#.to_string(),
                expected: TestValue::Null,
            },
            Test {
                input: r#"let key = "foo"; {"foo": 5}[key]"#.to_string(),
                expected: TestValue::Int(5),
            },
            Test {
                input: r#"{}["foo"]"#.to_string(),
                expected: TestValue::Null,
            },
            Test {
                input: r#"{5: 5}[5]"#.to_string(),
                expected: TestValue::Int(5),
            },
            Test {
                input: r#"{true: 5}[true]"#.to_string(),
                expected: TestValue::Int(5),
            },
            Test {
                input: r#"{false: 5}[false]"#.to_string(),
                expected: TestValue::Int(5),
            },
        ];

        for test in tests {
            let evaluated = eval_for_test(&test.input);
            check_object(&evaluated, &test.expected, &test.input);
        }
    }

    #[test]
    fn test_while_statement() {
        struct Test {
            input: String,
            expected: i64,
        }

        let input = "let i = 0; while(i < 10) { let i = i + 1;}".to_string();

        for test in tests {
            let evaluated = eval_for_test(&test.input);

            check_integer_object(&evaluated, test.expected, &test.input);
        }
    }
}
