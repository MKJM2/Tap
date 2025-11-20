use crate::ast::{Expression, FunctionDef, LiteralValue, Operator, Pattern, Program, Statement};
use crate::environment::Environment;
use crate::utils::gensym;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub args: Vec<String>,
    pub body: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInstance {
    pub name: String,
    pub fields: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub enum_name: String,
    pub variant_name: String,
    pub values: Vec<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub definition: Rc<FunctionDef>,
    pub env: Rc<RefCell<Environment>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Function(FunctionDef),
    Closure(Closure),
    Lambda(Lambda),
    Struct(Struct),
    StructInstance(StructInstance),
    Enum(Enum),
    EnumVariant(EnumVariant),
    Boolean(bool),
    List(Vec<Value>),
    Unit,
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(val) => write!(f, "{}", val),
            Value::Float(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::List(els) => {
                let elements: Vec<String> = els.iter().map(|e| format!("{}", e)).collect();
                write!(f, "[{}]", elements.join(", "))
            }
            Value::Unit => write!(f, "()"),
            Value::Null => write!(f, "null"),
            _ => write!(f, "<{}>", self.type_name()),
        }
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "str",
            Value::Boolean(_) => "bool",
            Value::List(_) => "list",
            Value::Unit => "unit",
            Value::Null => "null",
            Value::Function(_) | Value::Closure(_) | Value::Lambda(_) => "function",
            Value::Struct(_) => "struct_def",
            Value::StructInstance(_) => "struct_instance",
            Value::Enum(_) => "enum_def",
            Value::EnumVariant(_) => "enum_variant",
        }
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum RuntimeError {
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("Invalid access: {0}")]
    InvalidAccess(String),
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Division by zero")]
    DivisionByZero,
    #[error("No match found for value")]
    MatchError,
}

// Internal enum to handle control flow + values
enum StatementResult {
    Normal(Value),
    Return(Value),
    Break,
    Continue,
}

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter
    }

    pub fn interpret(
        &self,
        program: &Program,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Value>, RuntimeError> {
        let mut last_val = Value::Unit;

        for statement in &program.statements {
            match self.evaluate_statement(statement, &env)? {
                StatementResult::Return(val) => return Ok(Some(val)),
                StatementResult::Normal(val) => last_val = val,
                StatementResult::Break | StatementResult::Continue => {
                    // Top-level break/continue is usually invalid, but we'll ignore or error.
                    // For now, treating as normal no-op.
                }
            }
        }

        // If the last statement produced a value (e.g. `5;`), we return it.
        // This supports the test cases expecting implicit returns from scripts.
        if matches!(last_val, Value::Unit) {
            Ok(None)
        } else {
            Ok(Some(last_val))
        }
    }

    fn evaluate_statement(
        &self,
        statement: &Statement,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<StatementResult, RuntimeError> {
        match statement {
            Statement::Expression(expr) => self.evaluate_expression(expr, env),
            Statement::Assignment { name, value } => {
                let res = self.evaluate_expression(value, env)?;
                let val = match res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(res),
                };
                env.borrow_mut().set(name.clone(), val);
                Ok(StatementResult::Normal(Value::Unit))
            }
            Statement::VarDecl { name, value, .. } => {
                let initial_value = match value {
                    Some(expr) => {
                        let res = self.evaluate_expression(expr, env)?;
                        match res {
                            StatementResult::Normal(v) => v,
                            _ => return Ok(res),
                        }
                    }
                    None => Value::Null,
                };
                env.borrow_mut().define(name.clone(), initial_value);
                Ok(StatementResult::Normal(Value::Unit))
            }
            Statement::FunctionDef(def) => {
                let closure = Closure {
                    definition: Rc::new(def.clone()),
                    env: Rc::clone(env),
                };
                env.borrow_mut()
                    .define(def.name.clone(), Value::Closure(closure));
                Ok(StatementResult::Normal(Value::Unit))
            }
            Statement::StructDecl(decl) => {
                let struct_val = Value::Struct(Struct {
                    name: decl.name.clone(),
                    fields: decl.fields.iter().map(|(n, _)| n.clone()).collect(),
                });
                env.borrow_mut().set(decl.name.clone(), struct_val);
                Ok(StatementResult::Normal(Value::Unit))
            }
            Statement::EnumDecl(decl) => {
                let enum_val = Value::Enum(Enum {
                    name: decl.name.clone(),
                    variants: decl.variants.iter().map(|v| v.name.clone()).collect(),
                });
                env.borrow_mut().set(decl.name.clone(), enum_val);
                Ok(StatementResult::Normal(Value::Unit))
            }
            Statement::While { condition, body } => {
                let mut last_loop_val = Value::Unit;
                while {
                    let cond_res = self.evaluate_expression(condition, env)?;
                    let cond_val = match cond_res {
                        StatementResult::Normal(v) => v,
                        _ => return Ok(cond_res),
                    };
                    self.is_truthy(&cond_val)
                } {
                    match self.execute_block(body, env)? {
                        StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
                        StatementResult::Break => break,
                        StatementResult::Continue => continue,
                        StatementResult::Normal(v) => last_loop_val = v,
                    }
                }
                Ok(StatementResult::Normal(last_loop_val))
            }
            Statement::For {
                iterator,
                iterable,
                body,
            } => {
                let iter_res = self.evaluate_expression(iterable, env)?;
                let iter_val = match iter_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(iter_res),
                };
                let mut last_loop_val = Value::Unit;

                if let Value::List(elements) = iter_val {
                    for element in elements {
                        let loop_env =
                            Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(env))));
                        loop_env.borrow_mut().define(iterator.clone(), element);

                        match self.execute_block_with_env(body, &loop_env)? {
                            StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
                            StatementResult::Break => break,
                            StatementResult::Continue => continue,
                            StatementResult::Normal(v) => last_loop_val = v,
                        }
                    }
                    Ok(StatementResult::Normal(last_loop_val))
                } else {
                    Err(RuntimeError::TypeError("For loop expects a list".into()))
                }
            }
            Statement::Return(expr) => {
                let value = match expr {
                    Some(e) => {
                        let res = self.evaluate_expression(e, env)?;
                        match res {
                            StatementResult::Normal(v) => v,
                            _ => return Ok(res),
                        }
                    }
                    None => Value::Unit,
                };
                Ok(StatementResult::Return(value))
            }
            Statement::PropertyAssignment {
                object,
                property,
                value,
            } => {
                let object_name = if let Expression::Identifier(name) = object {
                    name
                } else {
                    return Err(RuntimeError::TypeError(
                        "Cannot assign to non-variable".into(),
                    ));
                };
                let res = self.evaluate_expression(value, env)?;
                let new_value = match res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(res),
                };
                let object_val = env
                    .borrow()
                    .get(object_name)
                    .ok_or(RuntimeError::UndefinedVariable(object_name.clone()))?;

                if let Value::StructInstance(mut instance) = object_val {
                    if instance.fields.contains_key(property) {
                        instance.fields.insert(property.clone(), new_value);
                        env.borrow_mut()
                            .set(object_name.clone(), Value::StructInstance(instance));
                        Ok(StatementResult::Normal(Value::Unit))
                    } else {
                        Err(RuntimeError::InvalidAccess(format!(
                            "Property {} not found",
                            property
                        )))
                    }
                } else {
                    Err(RuntimeError::TypeError(
                        "Target is not a struct instance".into(),
                    ))
                }
            }
            Statement::ArrayAssignment {
                array,
                index,
                value,
            } => {
                let array_expr = match array {
                    Expression::Identifier(name) => name,
                    _ => {
                        return Err(RuntimeError::TypeError(
                            "Array assignment only works on variables currently".into(),
                        ));
                    }
                };

                let array_val = env
                    .borrow()
                    .get(array_expr)
                    .ok_or(RuntimeError::UndefinedVariable(array_expr.clone()))?;
                
                let idx_res = self.evaluate_expression(index, env)?;
                let idx_val = match idx_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(idx_res),
                };
                
                let val_res = self.evaluate_expression(value, env)?;
                let new_val = match val_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(val_res),
                };

                if let (Value::List(mut list), Value::Integer(idx)) = (array_val, idx_val) {
                    let i = idx as usize;
                    if i < list.len() {
                        list[i] = new_val;
                        env.borrow_mut().set(array_expr.clone(), Value::List(list));
                        Ok(StatementResult::Normal(Value::Unit))
                    } else {
                        Err(RuntimeError::InvalidAccess("Index out of bounds".into()))
                    }
                } else {
                    Err(RuntimeError::TypeError("Invalid array assignment".into()))
                }
            }
            Statement::Break => Ok(StatementResult::Break),
            Statement::Continue => Ok(StatementResult::Continue),
        }
    }

    fn execute_block(
        &self,
        statements: &[Statement],
        env: &Rc<RefCell<Environment>>,
    ) -> Result<StatementResult, RuntimeError> {
        let block_env = Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(env))));
        self.execute_block_with_env(statements, &block_env)
    }

    fn execute_block_with_env(
        &self,
        statements: &[Statement],
        env: &Rc<RefCell<Environment>>,
    ) -> Result<StatementResult, RuntimeError> {
        let mut last_val = Value::Unit;
        for stmt in statements {
            match self.evaluate_statement(stmt, env)? {
                StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
                StatementResult::Break => return Ok(StatementResult::Break),
                StatementResult::Continue => return Ok(StatementResult::Continue),
                StatementResult::Normal(v) => last_val = v,
            }
        }
        Ok(StatementResult::Normal(last_val))
    }

    fn evaluate_expression(
        &self,
        expr: &Expression,
        env: &Rc<RefCell<Environment>>,
    ) -> Result<StatementResult, RuntimeError> {
        match expr {
            Expression::Literal(literal) => Ok(StatementResult::Normal(self.evaluate_literal(literal))),
            Expression::Identifier(name) => env
                .borrow()
                .get(name)
                .map(|v| StatementResult::Normal(v))
                .ok_or(RuntimeError::UndefinedVariable(name.clone())),
            Expression::Binary { left, op, right } => {
                let left_res = self.evaluate_expression(left, env)?;
                let left_val = match left_res {
                    StatementResult::Normal(v) => v,
                    // Propagate control flow signals
                    _ => return Ok(left_res),
                };

                match op {
                    Operator::Or => {
                        if self.is_truthy(&left_val) {
                            return Ok(StatementResult::Normal(Value::Boolean(true)));
                        }
                        let right_res = self.evaluate_expression(right, env)?;
                        return Ok(StatementResult::Normal(Value::Boolean(
                            self.is_truthy(&match right_res {
                                StatementResult::Normal(v) => v,
                                _ => return Ok(right_res),
                            }),
                        )));
                    }
                    Operator::And => {
                        if !self.is_truthy(&left_val) {
                            return Ok(StatementResult::Normal(Value::Boolean(false)));
                        }
                        let right_res = self.evaluate_expression(right, env)?;
                        return Ok(StatementResult::Normal(Value::Boolean(
                            self.is_truthy(&match right_res {
                                StatementResult::Normal(v) => v,
                                _ => return Ok(right_res),
                            }),
                        )));
                    }
                    _ => {}
                }
                let right_res = self.evaluate_expression(right, env)?;
                let right_val = match right_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(right_res),
                };
                let result = self.apply_binary_op(left_val, *op, right_val)?;
                Ok(StatementResult::Normal(result))
            }
            Expression::Unary { op, right } => {
                let right_res = self.evaluate_expression(right, env)?;
                let val = match right_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(right_res),
                };
                let result = match op {
                    Operator::Not => Value::Boolean(!self.is_truthy(&val)),
                    Operator::Subtract => match val {
                        Value::Integer(i) => Value::Integer(-i),
                        Value::Float(f) => Value::Float(-f),
                        _ => return Err(RuntimeError::TypeError("Negation requires number".into())),
                    },
                    _ => return Err(RuntimeError::TypeError("Invalid unary operator".into())),
                };
                Ok(StatementResult::Normal(result))
            }
            Expression::FunctionCall { callee, args } => {
                let func_res = self.evaluate_expression(callee, env)?;
                let func = match func_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(func_res),
                };

                let mut arg_vals = Vec::new();
                for arg in args {
                    let arg_res = self.evaluate_expression(arg, env)?;
                    let arg_val = match arg_res {
                        StatementResult::Normal(v) => v,
                        _ => return Ok(arg_res),
                    };
                    arg_vals.push(arg_val);
                }

                let result = match func {
                    Value::Closure(closure) => self.call_function(closure, arg_vals)?,
                    Value::Function(def) => {
                        let closure = Closure {
                            definition: Rc::new(def),
                            env: Rc::new(RefCell::new(Environment::new())),
                        };
                        self.call_function(closure, arg_vals)?
                    }
                    Value::EnumVariant(ev) => {
                        // This is an enum variant instantiation
                        Value::EnumVariant(EnumVariant {
                            enum_name: ev.enum_name,
                            variant_name: ev.variant_name,
                            values: arg_vals,
                        })
                    }
                    _ => return Err(RuntimeError::TypeError("Not a function".into())),
                };
                Ok(StatementResult::Normal(result))
            }
            Expression::Lambda { args, body } => {
                let func_def = FunctionDef {
                    name: gensym("lambda"),
                    args: args.clone(),
                    body: vec![Statement::Return(Some(*body.clone()))],
                };
                Ok(StatementResult::Normal(Value::Closure(Closure {
                    definition: Rc::new(func_def),
                    env: Rc::clone(env),
                })))
            }
            Expression::List(elements) => {
                let mut vals = Vec::new();
                for el in elements {
                    let el_res = self.evaluate_expression(el, env)?;
                    let el_val = match el_res {
                        StatementResult::Normal(v) => v,
                        _ => return Ok(el_res),
                    };
                    vals.push(el_val);
                }
                Ok(StatementResult::Normal(Value::List(vals)))
            }
            Expression::StructInstantiation { name, fields } => {
                let mut field_vals = HashMap::new();
                for (k, v_expr) in fields {
                    let v_res = self.evaluate_expression(v_expr, env)?;
                    let v = match v_res {
                        StatementResult::Normal(val) => val,
                        _ => return Ok(v_res),
                    };
                    field_vals.insert(k.clone(), v);
                }
                Ok(StatementResult::Normal(Value::StructInstance(
                    StructInstance {
                        name: name.clone(),
                        fields: field_vals,
                    },
                )))
            }
            Expression::Get { object, name } => {
                let obj_res = self.evaluate_expression(object, env)?;
                let obj = match obj_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(obj_res),
                };
                if let Value::StructInstance(inst) = obj {
                    inst.fields
                        .get(name)
                        .cloned()
                        .map(|v| StatementResult::Normal(v))
                        .ok_or(RuntimeError::InvalidAccess(format!("Field {}", name)))
                } else {
                    Err(RuntimeError::TypeError("Not a struct".into()))
                }
            }
            Expression::ArrayAccess { array, index } => {
                let arr_res = self.evaluate_expression(array, env)?;
                let arr = match arr_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(arr_res),
                };
                let idx_res = self.evaluate_expression(index, env)?;
                let idx = match idx_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(idx_res),
                };

                if let (Value::List(list), Value::Integer(i)) = (arr, idx) {
                    list.get(i as usize)
                        .cloned()
                        .map(|v| StatementResult::Normal(v))
                        .ok_or(RuntimeError::InvalidAccess("Index bounds".into()))
                } else {
                    Err(RuntimeError::TypeError("Invalid array access".into()))
                }
            }
            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_res = self.evaluate_expression(condition, env)?;
                let cond = match cond_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(cond_res),
                };

                if self.is_truthy(&cond) {
                    self.execute_block(then_branch, env)
                } else if let Some(else_stmt) = else_branch {
                    self.execute_block(else_stmt, env)
                } else {
                    Ok(StatementResult::Normal(Value::Unit))
                }
            }
            Expression::EnumVariant {
                enum_name,
                variant_name,
            } => {
                // Just return the value directly, assumes checks pass or done loosely
                Ok(StatementResult::Normal(Value::EnumVariant(EnumVariant {
                    enum_name: enum_name.clone(),
                    variant_name: variant_name.clone(),
                    values: vec![],
                })))
            }
            Expression::Path { parts } => {
                // Quick path implementation
                if parts.len() == 2 {
                    Ok(StatementResult::Normal(Value::EnumVariant(EnumVariant {
                        enum_name: parts[0].clone(),
                        variant_name: parts[1].clone(),
                        values: vec![],
                    })))
                } else {
                    Err(RuntimeError::TypeError("Invalid path".into()))
                }
            }
            Expression::Block(statements) => self.execute_block(&statements, env),
            Expression::Match { value, arms } => {
                let val_res = self.evaluate_expression(value, env)?;
                let val = match val_res {
                    StatementResult::Normal(v) => v,
                    _ => return Ok(val_res),
                };
                for arm in arms {
                    let match_env =
                        Rc::new(RefCell::new(Environment::new_enclosed(Rc::clone(env))));
                    if self.match_pattern(&val, &arm.pattern, &match_env) {
                        return self.evaluate_expression(&arm.body, &match_env);
                    }
                }
                Err(RuntimeError::MatchError)
            }
        }
    }

    fn call_function(&self, closure: Closure, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() != closure.definition.args.len() {
            return Err(RuntimeError::TypeError("Arg count mismatch".into()));
        }
        let call_env = Rc::new(RefCell::new(Environment::new_enclosed(closure.env)));
        for (name, val) in closure.definition.args.iter().zip(args) {
            call_env.borrow_mut().define(name.clone(), val);
        }

        match self.execute_block_with_env(&closure.definition.body, &call_env)? {
            StatementResult::Return(val) => Ok(val),
            StatementResult::Normal(val) => Ok(val), // Implicit return of last value
            _ => Ok(Value::Unit),
        }
    }

    fn evaluate_literal(&self, literal: &LiteralValue) -> Value {
        match literal {
            LiteralValue::Integer(i) => Value::Integer(*i),
            LiteralValue::Float(f) => Value::Float(*f),
            LiteralValue::String(s) => Value::String(s.clone()),
            LiteralValue::Boolean(b) => Value::Boolean(*b),
            LiteralValue::Unit => Value::Unit,
            LiteralValue::Array(elements) => {
                let vals = elements
                    .iter()
                    .map(|el| self.evaluate_literal(el))
                    .collect();
                Value::List(vals)
            }
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Boolean(b) => *b,
            Value::Null => false,
            Value::Unit => false,
            _ => true,
        }
    }

    fn match_pattern(
        &self,
        value: &Value,
        pattern: &Pattern,
        env: &Rc<RefCell<Environment>>,
    ) -> bool {
        match pattern {
            Pattern::Wildcard => true,
            Pattern::Literal(lit) => self.evaluate_literal(lit) == *value,
            Pattern::Identifier(p_name) => {
                if let Value::EnumVariant(v_val) = value {
                    // When matching an enum, an identifier pattern is treated as a variant name
                    p_name == &v_val.variant_name
                } else {
                    // Otherwise, it's a variable binding
                    env.borrow_mut().set(p_name.clone(), value.clone());
                    true
                }
            }
            Pattern::EnumVariant { variant, vars, .. } => {
                if let Value::EnumVariant(ev) = value {
                    if ev.variant_name == *variant && ev.values.len() == vars.len() {
                        for (i, sub_pattern) in vars.iter().enumerate() {
                            if !self.match_pattern(&ev.values[i], sub_pattern, env) {
                                return false; // a sub-pattern failed to match
                            }
                        }
                        true // all sub-patterns matched
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
        }
    }

    fn apply_binary_op(
        &self,
        left: Value,
        op: Operator,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Boolean(l), Value::Boolean(r)) => match op {
                Operator::Equal => Ok(Value::Boolean(l == r)),
                Operator::NotEqual => Ok(Value::Boolean(l != r)),
                _ => Err(RuntimeError::TypeError("Invalid boolean operator".into())),
            },
            (Value::Integer(l), Value::Integer(r)) => match op {
                Operator::Add => Ok(Value::Integer(l + r)),
                Operator::Subtract => Ok(Value::Integer(l - r)),
                Operator::Multiply => Ok(Value::Integer(l * r)),
                Operator::Divide => {
                    if r == 0 {
                        return Err(RuntimeError::DivisionByZero);
                    }
                    Ok(Value::Integer(l / r))
                }
                Operator::Modulo => {
                    if r == 0 {
                        return Err(RuntimeError::DivisionByZero);
                    }
                    Ok(Value::Integer(l % r))
                }
                Operator::Equal => Ok(Value::Boolean(l == r)),
                Operator::NotEqual => Ok(Value::Boolean(l != r)),
                Operator::GreaterThan => Ok(Value::Boolean(l > r)),
                Operator::LessThan => Ok(Value::Boolean(l < r)),
                Operator::GreaterThanEqual => Ok(Value::Boolean(l >= r)),
                Operator::LessThanEqual => Ok(Value::Boolean(l <= r)),
                _ => Err(RuntimeError::TypeError("Invalid integer operator".into())),
            },
            (Value::String(l), Value::String(r)) => match op {
                Operator::Add => Ok(Value::String(format!("{}{}", l, r))),
                Operator::Equal => Ok(Value::Boolean(l == r)),
                Operator::NotEqual => Ok(Value::Boolean(l != r)),
                _ => Err(RuntimeError::TypeError("Invalid string operator".into())),
            },
            (Value::Float(l), Value::Float(r)) => match op {
                Operator::Add => Ok(Value::Float(l + r)),
                Operator::Subtract => Ok(Value::Float(l - r)),
                Operator::Multiply => Ok(Value::Float(l * r)),
                Operator::Divide => Ok(Value::Float(l / r)),
                Operator::Equal => Ok(Value::Boolean(l == r)),
                Operator::LessThan => Ok(Value::Boolean(l < r)),
                _ => Err(RuntimeError::TypeError("Invalid float operator".into())),
            },
            _ => Err(RuntimeError::TypeError("Type mismatch in binary op".into())),
        }
    }
}
