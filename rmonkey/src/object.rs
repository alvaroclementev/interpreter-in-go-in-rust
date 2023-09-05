//! The internal representation of values inside the rmonkey interpreter
#![allow(dead_code)]

use itertools::Itertools;
use std::{cell::RefCell, fmt::Display, rc::Rc};

use crate::{ast, evaluator::Environment};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Null,
    Boolean(bool),
    Integer(i64),
    String(String),
    Return(Rc<Object>),
    Error(String),
    Function(Box<Function>),
    Builtin(BuiltinFunction),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Integer(val) => write!(f, "{}", val),
            Object::String(val) => write!(f, "{}", val),
            Object::Return(val) => write!(f, "{}", val),
            Object::Error(msg) => {
                // TODO(alvaro): Proper displaying of error messages
                write!(f, "error: {}", msg)
            }
            Object::Function(fun) => {
                let params = fun.parameters.iter().map(|p| format!("{}", p)).join(", ");
                write!(f, "fn ({}) {{\n{}\n}}", params, fun.body)
            }
            Object::Builtin(..) => write!(f, "builtin function"),
        }
    }
}

impl Object {
    pub fn as_boolean(&self) -> Object {
        match self {
            Object::Null => Object::Boolean(false),
            Object::Boolean(val) => Object::Boolean(*val),
            Object::Integer(val) => Object::Boolean(*val != 0),
            Object::String(..) => Object::Boolean(true),
            Object::Return(obj) => obj.as_boolean(),
            obj @ Object::Error(..) => obj.clone(),
            Object::Function(..) => Object::Boolean(true), // Functions are truthy I guess
            Object::Builtin(..) => Object::Boolean(true),  // Functions are truthy I guess
        }
    }

    pub fn is_truthy(&self) -> bool {
        let Object::Boolean(val) = self.as_boolean() else {
            unreachable!()
        };
        val
    }

    pub fn type_str(&self) -> &str {
        match self {
            Object::Null => "NULL",
            Object::Boolean(..) => "BOOLEAN",
            Object::Integer(..) => "INTEGER",
            Object::String(..) => "STRING",
            Object::Return(..) => "RETURN_VALUE",
            Object::Error(..) => "ERROR",
            Object::Function(..) => "FUNCTION",
            Object::Builtin(..) => "BUILTIN",
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(..))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<ast::Identifier>,
    pub body: ast::Block,
    pub environment: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(
        parameters: Vec<ast::Identifier>,
        body: ast::Block,
        environment: Rc<RefCell<Environment>>,
    ) -> Self {
        Self {
            parameters,
            body,
            environment,
        }
    }
}

/// A wrapper over a builtin function
///
/// It's separated to be able to implement common traits (Debug, Eq) on it
/// manually, so that Object stays simpler
#[derive(Clone)]
pub struct BuiltinFunction {
    pub name: String,
    pub param_count: Option<u8>,
    pub function: Rc<dyn Fn(Vec<Object>) -> Object>,
}

impl PartialEq for BuiltinFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.param_count == other.param_count
    }
}
impl Eq for BuiltinFunction {}

impl BuiltinFunction {
    pub fn new(
        name: String,
        param_count: Option<u8>,
        function: Rc<dyn Fn(Vec<Object>) -> Object>,
    ) -> Self {
        Self {
            name,
            param_count,
            function,
        }
    }

    pub fn call(&self, parameters: Vec<Object>) -> Object {
        // Check the number of params
        if let Some(count) = &self.param_count {
            if parameters.len() != *count as usize {
                return Object::Error(format!(
                    "wrong number of arguments. got={}, want={}",
                    parameters.len(),
                    count
                ));
            }
        }

        // Actually call the function
        (*self.function)(parameters)
    }
}

impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltinFunction")
            .field("name", &self.name)
            .field("param_count", &self.param_count)
            .field("function", &"<function>")
            .finish()
    }
}
