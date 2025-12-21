use crate::interpreter::{Interpreter, MapKey, RuntimeError, Value};
use crate::types::{SymbolInfo, Type};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Read, Write};
use std::rc::Rc;

/// Registry of all built-in functions and variables with their type signatures
pub struct BuiltinRegistry {
    pub global_functions: HashMap<String, Type>,
    pub global_variables: HashMap<String, SymbolInfo>,
}

impl BuiltinRegistry {
    pub fn new() -> Self {
        let mut global_functions = HashMap::new();
        let mut global_variables = HashMap::new();

        // === GLOBAL FUNCTIONS ===
        global_functions.insert(
            "print".to_string(),
            Type::Function(vec![Type::Any], Box::new(Type::Unit)),
        );

        global_functions.insert(
            "eprint".to_string(),
            Type::Function(vec![Type::Any], Box::new(Type::Unit)),
        );

        global_functions.insert(
            "Map".to_string(),
            Type::Function(
                vec![],
                Box::new(Type::Map(Box::new(Type::Unknown), Box::new(Type::Unknown))),
            ),
        );

        // TODO: read, read_lines, write, close
        global_functions.insert(
            "open".to_string(),
            Type::Function(
                vec![Type::String, Type::String],
                Box::new(Type::Any), // File type
            ),
        );

        // === MATH FUNCTIONS ===
        global_functions.insert(
            "sqrt".to_string(),
            Type::Function(vec![Type::Float, Type::Float], Box::new(Type::Float)),
        );

        // === GLOBAL VARIABLES ===

        // args built-in
        global_variables.insert(
            "args".to_string(),
            SymbolInfo {
                ty: Self::get_args_type(),
                mutable: false,
            },
        );

        BuiltinRegistry {
            global_functions,
            global_variables,
        }
    }

    /// Returns the type signature for the built-in `args` object
    fn get_args_type() -> Type {
        Type::Record(HashMap::from([
            // Properties (direct access)
            ("program".to_string(), Type::String),
            ("values".to_string(), Type::List(Box::new(Type::String))),
            ("length".to_string(), Type::Int),
            // Methods (require function call)
            (
                "get".to_string(),
                Type::Function(vec![Type::Int], Box::new(Type::String)),
            ),
            (
                "has".to_string(),
                Type::Function(vec![Type::String], Box::new(Type::Bool)),
            ),
            (
                "get_option".to_string(),
                Type::Function(vec![Type::String], Box::new(Type::String)),
            ),
        ]))
    }
}

impl Default for BuiltinRegistry {
    fn default() -> Self {
        Self::new()
    }
}

pub fn eval_method(
    interp: &mut Interpreter,
    receiver: Value,
    method: &str,
    mut args: Vec<Value>,
    _var_name: Option<&str>,
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

    match receiver {
        // ==================== MAP METHODS ====================
        Value::Map(map_rc) => {
            // We can mutate the map directly via map_rc.borrow_mut()
            match method {
                "insert" => {
                    check_arg_count(2)?;
                    let key = MapKey::from_value(&args[0])?;
                    map_rc.borrow_mut().insert(key, args[1].clone());
                    // Return the map itself (chainable) or Unit
                    Ok(Value::Map(map_rc.clone()))
                }
                "get" => {
                    check_arg_count(1)?;
                    let key = MapKey::from_value(&args[0])?;
                    let map = map_rc.borrow();
                    map.get(&key)
                        .cloned()
                        .ok_or_else(|| RuntimeError::Type(format!("Key {:?} not found", key)))
                }
                "has" | "contains" => {
                    check_arg_count(1)?;
                    let key = MapKey::from_value(&args[0])?;
                    Ok(Value::Boolean(map_rc.borrow().contains_key(&key)))
                }
                "remove" => {
                    check_arg_count(1)?;
                    let key = MapKey::from_value(&args[0])?;
                    let removed = map_rc
                        .borrow_mut()
                        .remove(&key)
                        .ok_or_else(|| RuntimeError::Type(format!("Key {:?} not found", key)))?;
                    Ok(removed)
                }
                "length" | "size" => Ok(Value::Integer(map_rc.borrow().len() as i64)),
                "is_empty" => Ok(Value::Boolean(map_rc.borrow().is_empty())),
                "clear" => {
                    map_rc.borrow_mut().clear();
                    Ok(Value::Map(map_rc.clone()))
                }
                "keys" => {
                    let map = map_rc.borrow();
                    let keys: Vec<Value> = map.keys().map(|k| k.to_value()).collect();
                    Ok(Value::List(Rc::new(RefCell::new(keys))))
                }
                "values" => {
                    let map = map_rc.borrow();
                    let values: Vec<Value> = map.values().cloned().collect();
                    Ok(Value::List(Rc::new(RefCell::new(values))))
                }
                "entries" => {
                    let entries: Vec<Value> = map_rc
                        .borrow()
                        .iter()
                        .map(|(k, v)| {
                            let mut fields = HashMap::new();
                            fields.insert("key".to_string(), k.to_value());
                            fields.insert("value".to_string(), v.clone());
                            Value::Record(fields)
                        })
                        .collect();
                    Ok(Value::List(Rc::new(RefCell::new(entries))))
                }
                _ => Err(RuntimeError::Type(format!(
                    "Unknown method '{}' for Map",
                    method
                ))),
            }
        }

        // ==================== LIST METHODS ====================
        Value::List(list_rc) => match method {
            "push" | "append" => {
                check_arg_count(1)?;
                // MUTATE IN PLACE
                list_rc.borrow_mut().push(args.swap_remove(0));
                Ok(Value::Unit)
            }
            "pop" => {
                let mut list = list_rc.borrow_mut();
                if list.is_empty() {
                    return Err(RuntimeError::Type("Cannot pop from empty list".into()));
                }
                Ok(list.pop().unwrap())
            }
            "remove" => {
                check_arg_count(1)?;
                if let Value::Integer(idx) = args[0] {
                    let mut list = list_rc.borrow_mut();
                    if idx < 0 || idx as usize >= list.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", idx)));
                    }
                    Ok(list.remove(idx as usize))
                } else {
                    Err(RuntimeError::Type("remove index must be integer".into()))
                }
            }
            "insert" => {
                check_arg_count(2)?;
                if let Value::Integer(idx) = args[0] {
                    let mut list = list_rc.borrow_mut();
                    if idx < 0 || idx as usize > list.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", idx)));
                    }
                    list.insert(idx as usize, args[1].clone());
                    Ok(Value::Unit)
                } else {
                    Err(RuntimeError::Type("insert index must be integer".into()))
                }
            }
            "reverse" => {
                list_rc.borrow_mut().reverse();
                Ok(Value::Unit)
            }
            "map" => {
                check_arg_count(1)?;
                let func = args[0].clone();

                // We must clone the elements first to release the borrow on list_rc.
                // Otherwise, if the closure tries to modify this list, it will panic.
                let elements: Vec<Value> = list_rc.borrow().clone();

                let mut results = Vec::with_capacity(elements.len());

                for elem in elements {
                    let result = interp.eval_function_call_value(func.clone(), &[elem])?;
                    results.push(result);
                }

                Ok(Value::List(Rc::new(RefCell::new(results))))
            }
            "filter" => {
                check_arg_count(1)?;
                let func = args[0].clone();

                // Snapshot the list to release the borrow
                let elements: Vec<Value> = list_rc.borrow().clone();

                let mut results = Vec::new();

                for elem in elements {
                    // Evaluate predicate
                    let keep = interp.eval_function_call_value(func.clone(), &[elem.clone()])?;

                    match keep {
                        Value::Boolean(b) => {
                            if b {
                                results.push(elem);
                            }
                        }
                        _ => {
                            return Err(RuntimeError::Type(
                                "filter predicate must return boolean".into(),
                            ));
                        }
                    }
                }

                Ok(Value::List(Rc::new(RefCell::new(results))))
            }
            "sort" => {
                // Sorting is trickier because we need to borrow the list to sort it,
                // but the comparator might need to call back into the interpreter.
                // If the comparator modifies THE SAME LIST, we panic (Double Mutable Borrow).
                // Usually safe to assume comparator doesn't mutate the list being sorted.

                // We extract the Vec temporarily to sort it to avoid borrow conflicts
                // if we were passing the list reference around, but here we can just borrow_mut.

                let mut list = list_rc.borrow_mut();

                if !args.is_empty() && matches!(args[0], Value::Function { .. }) {
                    let comparator_func = args.swap_remove(0);
                    let mut sort_error: Option<RuntimeError> = None;

                    // Note: We are holding a mutable borrow of `list` here.
                    // If `eval_function_call_value` tries to access `list` again, it will panic.
                    // This is a known limitation of this simple implementation.
                    list.sort_by(|a, b| {
                        if sort_error.is_some() {
                            return Ordering::Equal;
                        }

                        let res = interp.eval_function_call_value(
                            comparator_func.clone(),
                            &[a.clone(), b.clone()],
                        );

                        match res {
                            Ok(Value::Integer(i)) => {
                                if i < 0 {
                                    Ordering::Less
                                } else if i > 0 {
                                    Ordering::Greater
                                } else {
                                    Ordering::Equal
                                }
                            }
                            Ok(_) => {
                                sort_error = Some(RuntimeError::Type("Comp ret non-int".into()));
                                Ordering::Equal
                            }
                            Err(e) => {
                                sort_error = Some(e);
                                Ordering::Equal
                            }
                        }
                    });

                    if let Some(err) = sort_error {
                        return Err(err);
                    }
                    return Ok(Value::Unit);
                }

                // Default Sorts
                if list.iter().all(|v| matches!(v, Value::Integer(_))) {
                    list.sort_by(|a, b| {
                        if let (Value::Integer(x), Value::Integer(y)) = (a, b) {
                            x.cmp(y)
                        } else {
                            Ordering::Equal
                        }
                    });
                } else if list.iter().all(|v| matches!(v, Value::Float(_))) {
                    list.sort_by(|a, b| {
                        if let (Value::Float(x), Value::Float(y)) = (a, b) {
                            x.partial_cmp(y).unwrap_or(Ordering::Equal)
                        } else {
                            Ordering::Equal
                        }
                    });
                } else {
                    // ... other types ...
                }
                Ok(Value::Unit)
            }
            "length" => Ok(Value::Integer(list_rc.borrow().len() as i64)),
            "contains" => {
                check_arg_count(1)?;
                Ok(Value::Boolean(list_rc.borrow().contains(&args[0])))
            }
            "index_of" => {
                check_arg_count(1)?;
                match list_rc.borrow().iter().position(|v| v == &args[0]) {
                    Some(idx) => Ok(Value::Integer(idx as i64)),
                    None => Ok(Value::Integer(-1)),
                }
            }
            "slice" => {
                check_arg_count(2)?;
                let list = list_rc.borrow();
                match (&args[0], &args[1]) {
                    (Value::Integer(s), Value::Integer(e)) => {
                        let start = (*s).max(0) as usize;
                        let end = ((*e).max(0) as usize).min(list.len());
                        if start <= end {
                            let slice = list[start..end].to_vec();
                            Ok(Value::List(Rc::new(RefCell::new(slice))))
                        } else {
                            Err(RuntimeError::Type(format!(
                                "Invalid slice range {}..{}",
                                s, e
                            )))
                        }
                    }
                    _ => Err(RuntimeError::Type("slice args must be int".into())),
                }
            }
            // ... [Implement other list methods similarly using .borrow() or .borrow_mut()] ...
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
                    Ok(Value::List(Rc::new(RefCell::new(parts))))
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
                Ok(Value::List(Rc::new(RefCell::new(chars))))
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
                Ok(Value::List(Rc::new(RefCell::new(lines))))
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
            "values" => {
                let list: Vec<Value> = args_obj
                    .values
                    .iter()
                    .map(|s| Value::String(s.clone()))
                    .collect();
                Ok(Value::List(Rc::new(RefCell::new(list))))
            }
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
            Box::new(Type::Unit),
        )),
        "pop" => Some(Type::Function(vec![], Box::new(inner.clone()))),
        "remove" => Some(Type::Function(vec![Type::Int], Box::new(inner.clone()))),
        "insert" => Some(Type::Function(
            vec![Type::Int, inner.clone()],
            Box::new(Type::Unit),
        )),
        "reverse" => Some(Type::Function(
            vec![],
            Box::new(Type::List(Box::new(inner.clone()))),
        )),
        "sort" => Some(Type::Function(
            vec![],
            Box::new(Type::List(Box::new(inner.clone()))),
        )),
        "length" => Some(Type::Function(vec![], Box::new(Type::Int))),
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
        "is_empty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
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
        "length" | "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
        "is_empty" => Some(Type::Function(vec![], Box::new(Type::Bool))),
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
        "length" | "size" => Some(Type::Function(vec![], Box::new(Type::Int))),
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
