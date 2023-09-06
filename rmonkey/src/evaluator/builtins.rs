use std::rc::Rc;

use crate::object::{BuiltinFunction, Object};

pub fn get_builtin(name: &str) -> Option<BuiltinFunction> {
    // FIXME(alvaro): This is created a new reference on every call
    match name {
        "len" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(1),
            Rc::new(builtin_len),
        )),
        "first" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(1),
            Rc::new(builtin_first),
        )),
        "last" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(1),
            Rc::new(builtin_last),
        )),
        "rest" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(1),
            Rc::new(builtin_rest),
        )),
        "push" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(2),
            Rc::new(builtin_push),
        )),
        "puts" => Some(BuiltinFunction::new(
            name.to_string(),
            None,
            Rc::new(builtin_puts),
        )),
        _ => None,
    }
}

fn builtin_len(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::String(value) => Object::Integer(value.len() as i64),
        Object::Array(value) => Object::Integer(value.len() as i64),
        obj => Object::Error(format!(
            "argument to `len` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_first(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(value) => value.get(0).cloned().unwrap_or(Object::Null),
        obj => Object::Error(format!(
            "argument to `first` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_last(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(value) => {
            if value.is_empty() {
                Object::Null
            } else {
                value.last().unwrap().clone()
            }
        }
        obj => Object::Error(format!(
            "argument to `last` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_rest(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(value) => Object::Array(value.iter().skip(1).cloned().collect()),
        obj => Object::Error(format!(
            "argument to `rest` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_push(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Array(value) => {
            let mut new_arr = value.clone();
            new_arr.push(args[1].clone());
            Object::Array(new_arr)
        }
        obj => Object::Error(format!(
            "argument to `rest` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_puts(args: Vec<Object>) -> Object {
    for arg in args {
        println!("{}", arg);
    }
    Object::Null
}
