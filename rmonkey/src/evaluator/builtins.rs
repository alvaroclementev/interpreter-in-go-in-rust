use std::rc::Rc;

use crate::object::{BuiltinFunction, Object};

pub fn get_builtin(name: &str) -> Option<BuiltinFunction> {
    // FIXME(alvaro): This is created a new reference on every call
    match name {
        "contains" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(2),
            Rc::new(builtin_contains),
        )),
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
        "insert" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(3),
            Rc::new(builtin_insert),
        )),
        "keys" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(1),
            Rc::new(builtin_keys),
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
        "pairs" => Some(BuiltinFunction::new(
            name.to_string(),
            Some(1),
            Rc::new(builtin_pairs),
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
        "values" => Some(BuiltinFunction::new(
            name.to_string(),
            None,
            Rc::new(builtin_values),
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

// Array builtins
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

// Hash builtins
fn builtin_insert(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Hash(hash) => {
            let key = &args[1];
            if !key.is_hashable() {
                return Object::Error(format!("unusable as hash key: {}", key));
            }

            let mut new_hash = hash.clone();
            let value = &args[2];
            new_hash.insert(key.clone(), value.clone());
            Object::Hash(new_hash)
        }
        obj => Object::Error(format!(
            "first argument to `hash` not supported, got {}, expected HASH",
            obj.type_str()
        )),
    }
}

fn builtin_keys(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Hash(hash) => Object::Array(hash.keys().cloned().collect()),
        obj => Object::Error(format!(
            "argument to `keys` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_values(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Hash(hash) => Object::Array(hash.values().cloned().collect()),
        obj => Object::Error(format!(
            "argument to `values` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_pairs(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Hash(hash) => Object::Array(
            hash.iter()
                .map(|pair| Object::Array(vec![pair.0.clone(), pair.1.clone()]))
                .collect::<Vec<Object>>(),
        ),
        obj => Object::Error(format!(
            "argument to `pairs` not supported, got {}",
            obj.type_str()
        )),
    }
}

fn builtin_contains(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::Hash(hash) => {
            let key = &args[1];
            if !key.is_hashable() {
                return Object::Error(format!("unusable as hash key: {}", key));
            }
            Object::Boolean(hash.contains_key(key))
        }
        obj => Object::Error(format!(
            "argument to `keys` not supported, got {}",
            obj.type_str()
        )),
    }
}

// Other builtins

fn builtin_puts(args: Vec<Object>) -> Object {
    for arg in args {
        println!("{}", arg);
    }
    Object::Null
}
