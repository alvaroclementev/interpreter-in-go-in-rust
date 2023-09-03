//! The internal representation of values inside the rmonkey interpreter
#![allow(dead_code)]

use itertools::Itertools;
use std::{fmt::Display, rc::Rc, cell::RefCell};

use crate::{ast, evaluator::Environment};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Null,
    Boolean(bool),
    Integer(i64),
    Return(Rc<Object>),
    Error(String),
    Function(Box<Function>),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Null => write!(f, "null"),
            Object::Boolean(val) => write!(f, "{}", val),
            Object::Integer(val) => write!(f, "{}", val),
            Object::Return(val) => write!(f, "{}", val),
            Object::Error(msg) => {
                // TODO(alvaro): Proper displaying of error messages
                write!(f, "error: {}", msg)
            }
            Object::Function(fun) => {
                let params = fun.parameters.iter().map(|p| format!("{}", p)).join(", ");
                write!(f, "fn ({}) {{\n{}\n}}", params, fun.body)
            }
        }
    }
}

impl Object {
    pub fn as_boolean(&self) -> Object {
        match self {
            Object::Null => Object::Boolean(false),
            Object::Boolean(val) => Object::Boolean(*val),
            Object::Integer(val) => Object::Boolean(*val != 0),
            Object::Return(obj) => obj.as_boolean(),
            obj @ Object::Error(..) => obj.clone(),
            Object::Function(..) => Object::Boolean(true), // Functions are truthy I guess
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
            Object::Return(..) => "RETURN_VALUE",
            Object::Error(..) => "ERROR",
            Object::Function(..) => "FUNCTION",
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
