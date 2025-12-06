use crate::interpreter::{Interpreter, MapKey, RuntimeError, Value};
use crate::types::Type;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};

pub fn eval_method(
    interp: &mut Interpreter,
    receiver: Value,
    method: &str,
    args: Vec<Value>,
    var_name: Option<&str>,
) -> Result<Value, RuntimeError> {
    // Helper to enforce argument counts
    let check_arg_count = |expected: usize| -> Result<(), RuntimeError> {
        if args.len() != expected {
            Err(RuntimeError::Type(format!(
                "{} expects {} arguments",
                method, expected
            )))
        } else {
            Ok(())
        }
    };

    // Helper macro to handle mutation: update env and return the mutated structure
    macro_rules! mutate_and_return {
        ($new_val:expr) => {{
            let val = $new_val;
            if let Some(name) = var_name {
                interp.env.set(name, val.clone());
            }
            Ok(val)
        }};
    }

    // Helper macro for side-effects (pop/remove) that return an Item but modify the Collection in env
    macro_rules! mutate_side_effect {
        ($new_collection:expr, $return_item:expr) => {{
            if let Some(name) = var_name {
                interp.env.set(name, $new_collection);
            }
            Ok($return_item)
        }};
    }

    match receiver {
        // ==================== MAP METHODS ====================
        Value::Map(mut map) => match method {
            "insert" => {
                check_arg_count(2)?;
                let key = MapKey::from_value(&args[0])?;
                map.insert(key, args[1].clone());
                mutate_and_return!(Value::Map(map))
            }
            "get" => {
                check_arg_count(1)?;
                let key = MapKey::from_value(&args[0])?;
                map.get(&key)
                    .cloned()
                    .ok_or_else(|| RuntimeError::Type(format!("Key {:?} not found in map", key)))
            }
            "has" | "contains" => {
                check_arg_count(1)?;
                let key = MapKey::from_value(&args[0])?;
                Ok(Value::Boolean(map.contains_key(&key)))
            }
            "remove" => {
                check_arg_count(1)?;
                let key = MapKey::from_value(&args[0])?;
                let removed = map
                    .remove(&key)
                    .ok_or_else(|| RuntimeError::Type(format!("Key {:?} not found in map", key)))?;
                mutate_side_effect!(Value::Map(map), removed)
            }
            "length" | "size" => Ok(Value::Integer(map.len() as i64)),
            "is_empty" => Ok(Value::Boolean(map.is_empty())),
            "clear" => {
                mutate_and_return!(Value::Map(HashMap::new()))
            }
            "keys" => {
                let keys: Vec<Value> = map.keys().map(|k| k.to_value()).collect();
                Ok(Value::List(keys))
            }
            "values" => {
                let values: Vec<Value> = map.values().cloned().collect();
                Ok(Value::List(values))
            }
            "entries" => {
                let entries: Vec<Value> = map
                    .iter()
                    .map(|(k, v)| {
                        let mut fields = HashMap::new();
                        fields.insert("key".to_string(), k.to_value());
                        fields.insert("value".to_string(), v.clone());
                        Value::Record(fields)
                    })
                    .collect();
                Ok(Value::List(entries))
            }
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for Map",
                method
            ))),
        },

        // ==================== LIST METHODS ====================
        Value::List(mut list) => match method {
            "push" | "append" => {
                check_arg_count(1)?;
                list.push(args[0].clone());
                mutate_and_return!(Value::List(list))
            }
            "pop" => {
                if list.is_empty() {
                    return Err(RuntimeError::Type("Cannot pop from empty list".into()));
                }
                let popped = list.pop().unwrap();
                mutate_side_effect!(Value::List(list), popped)
            }
            "remove" => {
                check_arg_count(1)?;
                if let Value::Integer(idx) = args[0] {
                    if idx < 0 || idx as usize >= list.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", idx)));
                    }
                    let removed = list.remove(idx as usize);
                    mutate_side_effect!(Value::List(list), removed)
                } else {
                    Err(RuntimeError::Type("remove index must be integer".into()))
                }
            }
            "insert" => {
                check_arg_count(2)?;
                if let Value::Integer(idx) = args[0] {
                    if idx < 0 || idx as usize > list.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", idx)));
                    }
                    list.insert(idx as usize, args[1].clone());
                    mutate_and_return!(Value::List(list))
                } else {
                    Err(RuntimeError::Type("insert index must be integer".into()))
                }
            }
            "reverse" => {
                list.reverse();
                mutate_and_return!(Value::List(list))
            }
            "sort" => {
                // Sorting logic
                if list.iter().all(|v| matches!(v, Value::Integer(_))) {
                    list.sort_by(|a, b| {
                        if let (Value::Integer(x), Value::Integer(y)) = (a, b) {
                            x.cmp(y)
                        } else {
                            std::cmp::Ordering::Equal
                        }
                    });
                } else if list.iter().all(|v| matches!(v, Value::Float(_))) {
                    list.sort_by(|a, b| {
                        if let (Value::Float(x), Value::Float(y)) = (a, b) {
                            x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                        } else {
                            std::cmp::Ordering::Equal
                        }
                    });
                } else if list.iter().all(|v| matches!(v, Value::String(_))) {
                    list.sort_by(|a, b| {
                        if let (Value::String(x), Value::String(y)) = (a, b) {
                            x.cmp(y)
                        } else {
                            std::cmp::Ordering::Equal
                        }
                    });
                } else {
                    return Err(RuntimeError::Type(
                        "Cannot sort list with mixed or unsortable types".into(),
                    ));
                }
                mutate_and_return!(Value::List(list))
            }
            "length" => Ok(Value::Integer(list.len() as i64)),
            "contains" => {
                check_arg_count(1)?;
                Ok(Value::Boolean(list.contains(&args[0])))
            }
            "index_of" => {
                check_arg_count(1)?;
                match list.iter().position(|v| v == &args[0]) {
                    Some(idx) => Ok(Value::Integer(idx as i64)),
                    None => Ok(Value::Integer(-1)),
                }
            }
            "slice" => {
                check_arg_count(2)?;
                match (&args[0], &args[1]) {
                    (Value::Integer(s), Value::Integer(e)) => {
                        let start = (*s).max(0) as usize;
                        let end = ((*e).max(0) as usize).min(list.len());
                        if start <= end && start <= list.len() {
                            Ok(Value::List(list[start..end].to_vec()))
                        } else {
                            Err(RuntimeError::Type(format!(
                                "Invalid slice range {}..{}",
                                s, e
                            )))
                        }
                    }
                    _ => Err(RuntimeError::Type(
                        "slice arguments must be integers".into(),
                    )),
                }
            }
            "join" => {
                check_arg_count(1)?;
                if let Value::String(sep) = &args[0] {
                    let strings: Result<Vec<String>, _> = list
                        .iter()
                        .map(|v| {
                            if let Value::String(s) = v {
                                Ok(s.clone())
                            } else {
                                Err(RuntimeError::Type("join requires list of strings".into()))
                            }
                        })
                        .collect();
                    match strings {
                        Ok(strs) => Ok(Value::String(strs.join(sep))),
                        Err(e) => Err(e),
                    }
                } else {
                    Err(RuntimeError::Type("join separator must be string".into()))
                }
            }
            "map" => {
                check_arg_count(1)?;
                let func = args[0].clone();
                let mut results = Vec::new();
                for elem in list {
                    // Call back into Interpreter to evaluate closure!
                    let result = interp.eval_function_call_value(func.clone(), &[elem])?;
                    results.push(result);
                }
                Ok(Value::List(results))
            }
            "filter" => {
                check_arg_count(1)?;
                let func = args[0].clone();
                let mut results = Vec::new();
                for elem in list {
                    let keep = interp.eval_function_call_value(func.clone(), &[elem.clone()])?;
                    if let Value::Boolean(true) = keep {
                        results.push(elem);
                    } else if !matches!(keep, Value::Boolean(_)) {
                        return Err(RuntimeError::Type(
                            "filter predicate must return boolean".into(),
                        ));
                    }
                }
                Ok(Value::List(results))
            }
            "first" => list
                .first()
                .cloned()
                .ok_or_else(|| RuntimeError::Type("Cannot get first of empty list".into())),
            "last" => list
                .last()
                .cloned()
                .ok_or_else(|| RuntimeError::Type("Cannot get last of empty list".into())),
            "is_empty" => Ok(Value::Boolean(list.is_empty())),
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for List",
                method
            ))),
        },

        // ==================== STRING METHODS ====================
        Value::String(s) => match method {
            "length" => Ok(Value::Integer(s.len() as i64)),
            "split" => {
                check_arg_count(1)?;
                if let Value::String(d) = &args[0] {
                    let parts: Vec<Value> = s
                        .split(d.as_str())
                        .map(|p| Value::String(p.to_string()))
                        .collect();
                    Ok(Value::List(parts))
                } else {
                    Err(RuntimeError::Type("split delimiter must be string".into()))
                }
            }
            "parse_int" => s
                .trim()
                .parse::<i64>()
                .map(Value::Integer)
                .map_err(|_| RuntimeError::Type(format!("Cannot parse '{}' as integer", s))),
            "parse_float" => s
                .trim()
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| RuntimeError::Type(format!("Cannot parse '{}' as float", s))),
            "trim" => Ok(Value::String(s.trim().to_string())),
            "trim_start" => Ok(Value::String(s.trim_start().to_string())),
            "trim_end" => Ok(Value::String(s.trim_end().to_string())),
            "contains" => {
                check_arg_count(1)?;
                if let Value::String(n) = &args[0] {
                    Ok(Value::Boolean(s.contains(n.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "contains argument must be string".into(),
                    ))
                }
            }
            "starts_with" => {
                check_arg_count(1)?;
                if let Value::String(p) = &args[0] {
                    Ok(Value::Boolean(s.starts_with(p.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "starts_with argument must be string".into(),
                    ))
                }
            }
            "ends_with" => {
                check_arg_count(1)?;
                if let Value::String(suf) = &args[0] {
                    Ok(Value::Boolean(s.ends_with(suf.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "ends_with argument must be string".into(),
                    ))
                }
            }
            "replace" => {
                check_arg_count(2)?;
                if let (Value::String(f), Value::String(t)) = (&args[0], &args[1]) {
                    Ok(Value::String(s.replace(f.as_str(), t.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "replace arguments must be strings".into(),
                    ))
                }
            }
            "to_lower" => Ok(Value::String(s.to_lowercase())),
            "to_upper" => Ok(Value::String(s.to_uppercase())),
            "char_at" => {
                check_arg_count(1)?;
                if let Value::Integer(i) = args[0] {
                    if i < 0 || i as usize >= s.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", i)));
                    }
                    let ch = s.chars().nth(i as usize).unwrap();
                    Ok(Value::String(ch.to_string()))
                } else {
                    Err(RuntimeError::Type("char_at index must be integer".into()))
                }
            }
            "chars" => {
                let chars: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();
                Ok(Value::List(chars))
            }
            "index_of" => {
                check_arg_count(1)?;
                if let Value::String(n) = &args[0] {
                    match s.find(n.as_str()) {
                        Some(idx) => Ok(Value::Integer(idx as i64)),
                        None => Ok(Value::Integer(-1)),
                    }
                } else {
                    Err(RuntimeError::Type(
                        "index_of argument must be string".into(),
                    ))
                }
            }
            "substring" => {
                check_arg_count(2)?;
                match (&args[0], &args[1]) {
                    (Value::Integer(start), Value::Integer(len)) => {
                        let start = *start as usize;
                        let len = *len as usize;
                        let end = (start + len).min(s.len());
                        if start <= s.len() {
                            Ok(Value::String(s[start..end].to_string()))
                        } else {
                            Err(RuntimeError::Type(format!(
                                "Start index {} out of bounds",
                                start
                            )))
                        }
                    }
                    _ => Err(RuntimeError::Type(
                        "substring arguments must be integers".into(),
                    )),
                }
            }
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for String",
                method
            ))),
        },

        // ==================== INTEGER METHODS ====================
        Value::Integer(i) => match method {
            "to_float" => Ok(Value::Float(i as f64)),
            "to_string" => Ok(Value::String(i.to_string())),
            "abs" => Ok(Value::Integer(i.abs())),
            "pow" => {
                check_arg_count(1)?;
                if let Value::Integer(e) = args[0] {
                    if e < 0 {
                        return Err(RuntimeError::Type(
                            "pow exponent must be non-negative".into(),
                        ));
                    }
                    Ok(Value::Integer(i.pow(e as u32)))
                } else {
                    Err(RuntimeError::Type("pow exponent must be integer".into()))
                }
            }
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for Integer",
                method
            ))),
        },

        // ==================== FLOAT METHODS ====================
        Value::Float(f) => match method {
            "to_string" => Ok(Value::String(f.to_string())),
            "to_int" => Ok(Value::Integer(f as i64)),
            "abs" => Ok(Value::Float(f.abs())),
            "floor" => Ok(Value::Float(f.floor())),
            "ceil" => Ok(Value::Float(f.ceil())),
            "round" => Ok(Value::Float(f.round())),
            "sqrt" => Ok(Value::Float(f.sqrt())),
            "pow" => {
                check_arg_count(1)?;
                match args[0] {
                    Value::Float(e) => Ok(Value::Float(f.powf(e))),
                    Value::Integer(e) => Ok(Value::Float(f.powi(e as i32))),
                    _ => Err(RuntimeError::Type("pow exponent must be number".into())),
                }
            }
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for Float",
                method
            ))),
        },

        // ==================== BOOLEAN METHODS ====================
        Value::Boolean(b) => match method {
            "to_string" => Ok(Value::String(b.to_string())),
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for Boolean",
                method
            ))),
        },

        // ==================== FILE METHODS ====================
        Value::File { id, closed, .. } => match method {
            "read" => {
                if closed {
                    return Err(RuntimeError::Type("Cannot read from closed file".into()));
                }
                if id >= interp.files.len() || interp.files[id].is_none() {
                    return Err(RuntimeError::Type("Invalid file descriptor".into()));
                }
                let mut content = String::new();
                if let Some(file) = &mut interp.files[id] {
                    file.read_to_string(&mut content)
                        .map_err(|e| RuntimeError::Type(format!("Failed to read file: {}", e)))?;
                }
                Ok(Value::String(content))
            }
            "read_lines" => {
                if closed {
                    return Err(RuntimeError::Type("Cannot read from closed file".into()));
                }
                if id >= interp.files.len() || interp.files[id].is_none() {
                    return Err(RuntimeError::Type("Invalid file descriptor".into()));
                }
                let lines: Vec<Value> = if let Some(file) = &interp.files[id] {
                    BufReader::new(file)
                        .lines()
                        .collect::<Result<Vec<_>, _>>()
                        .map_err(|e| RuntimeError::Type(format!("Failed to read lines: {}", e)))?
                        .into_iter()
                        .map(Value::String)
                        .collect()
                } else {
                    Vec::new()
                };
                Ok(Value::List(lines))
            }
            "write" => {
                check_arg_count(1)?;
                if closed {
                    return Err(RuntimeError::Type("Cannot write to closed file".into()));
                }
                if id >= interp.files.len() || interp.files[id].is_none() {
                    return Err(RuntimeError::Type("Invalid file descriptor".into()));
                }
                let text_data = interp.value_to_display_string(&args[0]);
                if let Some(file) = &mut interp.files[id] {
                    file.write_all(text_data.as_bytes()).map_err(|e| {
                        RuntimeError::Type(format!("Failed to write to file: {}", e))
                    })?;
                }
                Ok(Value::Unit)
            }
            "write_line" => {
                check_arg_count(1)?;
                if closed {
                    return Err(RuntimeError::Type("Cannot write to closed file".into()));
                }
                if id >= interp.files.len() || interp.files[id].is_none() {
                    return Err(RuntimeError::Type("Invalid file descriptor".into()));
                }
                let text_data = format!("{}\n", interp.value_to_display_string(&args[0]));
                if let Some(file) = &mut interp.files[id] {
                    file.write_all(text_data.as_bytes()).map_err(|e| {
                        RuntimeError::Type(format!("Failed to write to file: {}", e))
                    })?;
                }
                Ok(Value::Unit)
            }
            "close" => {
                if id >= interp.files.len() {
                    return Err(RuntimeError::Type("Invalid file descriptor".into()));
                }
                interp.files[id] = None;
                Ok(Value::Unit)
            }
            "is_closed" => Ok(Value::Boolean(closed)),
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for File",
                method
            ))),
        },

        // ==================== ARGS METHODS ====================
        Value::Args(args_obj) => match method {
            "program" => Ok(Value::String(args_obj.program.clone())),
            "values" => Ok(Value::List(
                args_obj
                    .values
                    .iter()
                    .map(|s| Value::String(s.clone()))
                    .collect(),
            )),
            "length" => Ok(Value::Integer(args_obj.values.len() as i64)),
            "get" => {
                check_arg_count(1)?;
                if let Value::Integer(i) = args[0] {
                    if i < 0 || i as usize >= args_obj.values.len() {
                        return Ok(Value::Unit);
                    }
                    Ok(Value::String(args_obj.values[i as usize].clone()))
                } else {
                    Err(RuntimeError::Type("args.get index must be integer".into()))
                }
            }
            "has" => {
                check_arg_count(1)?;
                if let Value::String(f) = &args[0] {
                    Ok(Value::Boolean(args_obj.flags.contains_key(f)))
                } else {
                    Err(RuntimeError::Type(
                        "args.has argument must be string".into(),
                    ))
                }
            }
            "get_option" => {
                check_arg_count(1)?;
                if let Value::String(k) = &args[0] {
                    match args_obj.options.get(k) {
                        Some(v) => Ok(Value::String(v.clone())),
                        None => Ok(Value::Unit),
                    }
                } else {
                    Err(RuntimeError::Type(
                        "args.get_option argument must be string".into(),
                    ))
                }
            }
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for Args",
                method
            ))),
        },

        _ => Err(RuntimeError::Type(format!(
            "Unknown method '{}' for type {:?}",
            method,
            std::mem::discriminant(&receiver)
        ))),
    }
}

/// Returns the type signature of a built-in method for a given receiver type.
/// Returns None if the method doesn't exist for that type.
///
/// For zero-argument methods (like `length`), returns the direct result type.
/// For methods with arguments, returns a Function type.
pub fn get_builtin_method_type(receiver_ty: &Type, method_name: &str) -> Option<Type> {
    // Allow any method on Unknown/Any types - return Any
    if matches!(receiver_ty, Type::Unknown | Type::Any) {
        return Some(Type::Any);
    }

    match receiver_ty {
        Type::List(inner) => get_list_method_type(inner, method_name),
        Type::Map(key, value) => get_map_method_type(key, value, method_name),
        Type::String => get_string_method_type(method_name),
        Type::Int => get_int_method_type(method_name),
        Type::Float => get_float_method_type(method_name),
        Type::Bool => get_bool_method_type(method_name),
        _ => None,
    }
}

fn get_list_method_type(inner: &Type, method: &str) -> Option<Type> {
    match method {
        "push" | "append" => Some(Type::Function(
            vec![inner.clone()],
            Box::new(Type::List(Box::new(inner.clone()))),
        )),
        "pop" => Some(Type::Function(vec![], Box::new(inner.clone()))),
        "remove" => Some(Type::Function(vec![Type::Int], Box::new(inner.clone()))),
        "insert" => Some(Type::Function(
            vec![Type::Int, inner.clone()],
            Box::new(Type::List(Box::new(inner.clone()))),
        )),
        "reverse" => Some(Type::Function(
            vec![],
            Box::new(Type::List(Box::new(inner.clone()))),
        )),
        "sort" => Some(Type::Function(
            vec![],
            Box::new(Type::List(Box::new(inner.clone()))),
        )),
        "length" => Some(Type::Int),
        "contains" => Some(Type::Function(vec![inner.clone()], Box::new(Type::Bool))),
        "index_of" => Some(Type::Function(vec![inner.clone()], Box::new(Type::Int))),
        "slice" => Some(Type::Function(
            vec![Type::Int, Type::Int],
            Box::new(Type::List(Box::new(inner.clone()))),
        )),
        "join" => {
            // join only works on List<String>
            if matches!(inner, Type::String) {
                Some(Type::Function(vec![Type::String], Box::new(Type::String)))
            } else {
                None
            }
        }
        "map" => {
            // map: (T -> U) -> List<U>
            // For simplicity, we'll use Any for the result type
            Some(Type::Function(
                vec![Type::Function(vec![inner.clone()], Box::new(Type::Any))],
                Box::new(Type::List(Box::new(Type::Any))),
            ))
        }
        "filter" => {
            // filter: (T -> Bool) -> List<T>
            Some(Type::Function(
                vec![Type::Function(vec![inner.clone()], Box::new(Type::Bool))],
                Box::new(Type::List(Box::new(inner.clone()))),
            ))
        }
        "first" | "last" => Some(Type::Function(vec![], Box::new(inner.clone()))),
        "is_empty" => Some(Type::Bool),
        _ => None,
    }
}

fn get_map_method_type(key_ty: &Type, val_ty: &Type, method: &str) -> Option<Type> {
    match method {
        "insert" => Some(Type::Function(
            vec![key_ty.clone(), val_ty.clone()],
            Box::new(Type::Map(
                Box::new(key_ty.clone()),
                Box::new(val_ty.clone()),
            )),
        )),
        "get" => Some(Type::Function(
            vec![key_ty.clone()],
            Box::new(val_ty.clone()),
        )),
        "has" | "contains" => Some(Type::Function(vec![key_ty.clone()], Box::new(Type::Bool))),
        "remove" => Some(Type::Function(
            vec![key_ty.clone()],
            Box::new(val_ty.clone()),
        )),
        "length" | "size" => Some(Type::Int),
        "is_empty" => Some(Type::Bool),
        "clear" => Some(Type::Function(
            vec![],
            Box::new(Type::Map(
                Box::new(key_ty.clone()),
                Box::new(val_ty.clone()),
            )),
        )),
        "keys" => Some(Type::Function(
            vec![],
            Box::new(Type::List(Box::new(key_ty.clone()))),
        )),
        "values" => Some(Type::Function(
            vec![],
            Box::new(Type::List(Box::new(val_ty.clone()))),
        )),
        "entries" => {
            let entry_record = Type::Record(HashMap::from([
                ("key".to_string(), key_ty.clone()),
                ("value".to_string(), val_ty.clone()),
            ]));
            Some(Type::Function(
                vec![],
                Box::new(Type::List(Box::new(entry_record))),
            ))
        }
        _ => None,
    }
}

fn get_string_method_type(method: &str) -> Option<Type> {
    match method {
        "length" => Some(Type::Int),
        "split" => Some(Type::Function(
            vec![Type::String],
            Box::new(Type::List(Box::new(Type::String))),
        )),
        "parse_int" => Some(Type::Function(vec![], Box::new(Type::Int))),
        "parse_float" => Some(Type::Function(vec![], Box::new(Type::Float))),
        "trim" | "trim_start" | "trim_end" | "to_lower" | "to_upper" => {
            Some(Type::Function(vec![], Box::new(Type::String)))
        }
        "contains" | "starts_with" | "ends_with" => {
            Some(Type::Function(vec![Type::String], Box::new(Type::Bool)))
        }
        "replace" => Some(Type::Function(
            vec![Type::String, Type::String],
            Box::new(Type::String),
        )),
        "char_at" => Some(Type::Function(vec![Type::Int], Box::new(Type::String))),
        "chars" => Some(Type::Function(
            vec![],
            Box::new(Type::List(Box::new(Type::String))),
        )),
        "index_of" => Some(Type::Function(vec![Type::String], Box::new(Type::Int))),
        "substring" => Some(Type::Function(
            vec![Type::Int, Type::Int],
            Box::new(Type::String),
        )),
        _ => None,
    }
}

fn get_int_method_type(method: &str) -> Option<Type> {
    match method {
        "to_float" => Some(Type::Function(vec![], Box::new(Type::Float))),
        "to_string" => Some(Type::Function(vec![], Box::new(Type::String))),
        "abs" => Some(Type::Function(vec![], Box::new(Type::Int))),
        "pow" => Some(Type::Function(vec![Type::Int], Box::new(Type::Int))),
        _ => None,
    }
}

fn get_float_method_type(method: &str) -> Option<Type> {
    match method {
        "to_string" => Some(Type::Function(vec![], Box::new(Type::String))),
        "to_int" => Some(Type::Function(vec![], Box::new(Type::Int))),
        "abs" | "floor" | "ceil" | "round" | "sqrt" => {
            Some(Type::Function(vec![], Box::new(Type::Float)))
        }
        "pow" => {
            // Can take Int or Float
            Some(Type::Function(vec![Type::Any], Box::new(Type::Float)))
        }
        _ => None,
    }
}

fn get_bool_method_type(method: &str) -> Option<Type> {
    match method {
        "to_string" => Some(Type::Function(vec![], Box::new(Type::String))),
        _ => None,
    }
}
