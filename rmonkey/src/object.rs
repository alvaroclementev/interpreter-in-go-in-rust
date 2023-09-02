//! The internal representation of values inside the rmonkey interpreter
#![allow(dead_code)]

use std::{fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Object {
    Null,
    Boolean(bool),
    Integer(i64),
    Return(Rc<Object>),
    Error(String),
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
        }
    }

    pub fn is_error(&self) -> bool {
        matches!(self, Object::Error(..))
    }
}
