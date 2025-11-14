use crate::ast::{Expression, FunctionDef, LiteralValue, Operator, Program, Statement, StructDecl};
use crate::environment::Environment;
use std::collections::HashMap;
use std::fmt;

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

/// Represents a runtime value in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    String(String),
    Function(FunctionDef),
    Lambda(Lambda),
    Struct(Struct),
    StructInstance(StructInstance),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(val) => write!(f, "{} : int", val),
            Value::String(val) => write!(f, "{} : string", val),
            Value::Function(def) => write!(f, "{} ({}) {{}}", def.name, def.args.join(", ")),
            Value::Lambda(_) => write!(f, "<lambda>"),
            Value::Struct(s) => write!(f, "<struct {}>", s.name),
            Value::StructInstance(s) => write!(f, "<instance of {}>", s.name),
            Value::Null => write!(f, "null"),
        }
    }
}

/// Represents an error that can occur during interpretation.
#[derive(Debug, Clone, PartialEq)]
pub enum RuntimeError {
    UndefinedVariable(String),
    TypeError(String),
}

/// The interpreter, responsible for evaluating the AST.
pub struct Interpreter;

impl Interpreter {
    /// Creates a new `Interpreter`.
    pub fn new() -> Self {
        Interpreter
    }

    /// Interprets a program.
    pub fn interpret(
        &self,
        program: &Program,
        env: &mut Environment,
    ) -> Result<Value, RuntimeError> {
        let mut result = Value::Null;
        for statement in &program.statements {
            result = self.evaluate_statement(statement, env)?;
        }
        Ok(result)
    }

    fn evaluate_statement(
        &self,
        statement: &Statement,
        env: &mut Environment,
    ) -> Result<Value, RuntimeError> {
        match statement {
            Statement::Expression(expr) => self.evaluate_expression(expr, env),
            Statement::Assignment { name, value } => {
                let value = self.evaluate_expression(value, env)?;
                env.set(name.clone(), value.clone());
                Ok(value)
            }
            Statement::VarDecl { name, .. } => {
                env.set(name.clone(), Value::Null);
                Ok(Value::Null)
            }
            Statement::FunctionDef(def) => {
                env.set(def.name.clone(), Value::Function(def.clone()));
                Ok(Value::Null)
            }
            Statement::StructDecl(decl) => {
                let name = decl.name.clone();
                let fields = decl.fields.iter().map(|(name, _)| name.clone()).collect();
                let struct_val = Value::Struct(Struct { name, fields });
                env.set(decl.name.clone(), struct_val);
                Ok(Value::Null)
            }
            Statement::EnumDecl(_) => {
                // TODO: Implement enum declaration
                Ok(Value::Null)
            }
        }
    }

    fn evaluate_expression(
        &self,
        expr: &Expression,
        env: &Environment,
    ) -> Result<Value, RuntimeError> {
        match expr {
            Expression::Literal(literal) => Ok(self.evaluate_literal(literal)),
            Expression::Identifier(name) => env
                .get(name)
                .cloned()
                .ok_or_else(|| RuntimeError::UndefinedVariable(name.clone())),
            Expression::Binary { left, op, right } => {
                let left = self.evaluate_expression(left, env)?;
                let right = self.evaluate_expression(right, env)?;
                self.apply_binary_op(left, *op, right)
            }
            Expression::FunctionCall { callee, args } => {
                let function = self.evaluate_expression(callee, env)?;

                match function {
                    Value::Function(def) => {
                        let mut func_env = Environment::new_enclosed(env);
                        if def.args.len() != args.len() {
                            return Err(RuntimeError::TypeError(format!(
                                "Expected {} arguments but got {}",
                                def.args.len(),
                                args.len()
                            )));
                        }
                        for (param, arg) in def.args.iter().zip(args) {
                            let arg_val = self.evaluate_expression(arg, env)?;
                            func_env.set(param.clone(), arg_val);
                        }

                        let mut result = Value::Null;
                        for statement in &def.body {
                            result = self.evaluate_statement(statement, &mut func_env)?;
                        }
                        Ok(result)
                    }
                    Value::Lambda(lambda) => {
                        let mut lambda_env = Environment::new_enclosed(env);
                        if lambda.args.len() != args.len() {
                            return Err(RuntimeError::TypeError(format!(
                                "Expected {} arguments but got {}",
                                lambda.args.len(),
                                args.len()
                            )));
                        }
                        for (param, arg) in lambda.args.iter().zip(args) {
                            let arg_val = self.evaluate_expression(arg, env)?;
                            lambda_env.set(param.clone(), arg_val);
                        }

                        self.evaluate_expression(&lambda.body, &lambda_env)
                    }
                    _ => Err(RuntimeError::TypeError(format!(
                        "{:?} is not a function",
                        callee
                    ))),
                }
            }
            Expression::Lambda { args, body } => Ok(Value::Lambda(Lambda {
                args: args.clone(),
                body: body.clone(),
            })),
            Expression::StructInstantiation { name, fields } => {
                let struct_val = env.get(name).cloned().ok_or_else(|| {
                    RuntimeError::TypeError(format!("{} is not a struct", name))
                })?;

                if let Value::Struct(struct_def) = struct_val {
                    let mut instance_fields = HashMap::new();
                    for (field_name, field_value) in fields {
                        let value = self.evaluate_expression(field_value, env)?;
                        instance_fields.insert(field_name.clone(), value);
                    }

                    Ok(Value::StructInstance(StructInstance {
                        name: name.clone(),
                        fields: instance_fields,
                    }))
                } else {
                    Err(RuntimeError::TypeError(format!("{} is not a struct", name)))
                }
            }
            Expression::Get { object, name } => {
                let object = self.evaluate_expression(object, env)?;
                if let Value::StructInstance(instance) = object {
                    instance.fields.get(name).cloned().ok_or_else(|| {
                        RuntimeError::TypeError(format!("Undefined property {}", name))
                    })
                } else {
                    Err(RuntimeError::TypeError(
                        "Only struct instances have properties.".to_string(),
                    ))
                }
            }
            _ => unimplemented!(),
        }
    }

    fn evaluate_literal(&self, literal: &LiteralValue) -> Value {
        match literal {
            LiteralValue::Integer(val) => Value::Integer(*val),
            LiteralValue::String(val) => Value::String(val.clone()),
        }
    }

    fn apply_binary_op(
        &self,
        left: Value,
        op: Operator,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Integer(left), Value::Integer(right)) => match op {
                Operator::Add => Ok(Value::Integer(left + right)),
                Operator::Subtract => Ok(Value::Integer(left - right)),
                Operator::Multiply => Ok(Value::Integer(left * right)),
                Operator::Divide => Ok(Value::Integer(left / right)),
            },
            (Value::String(left), Value::String(right)) => match op {
                Operator::Add => Ok(Value::String(format!("{}{}", left, right))),
                _ => Err(RuntimeError::TypeError(format!(
                    "Unsupported operator {:?} for strings",
                    op
                ))),
            },
            (l, r) => Err(RuntimeError::TypeError(format!(
                "Cannot apply operator {:?} to {:?} and {:?}",
                op, l, r
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn interpret_source(source: &str) -> Result<Value, RuntimeError> {
        let tokens = Lexer::new(source).tokenize().unwrap();
        let mut parser = Parser::new(&tokens);
        let program = parser.parse_program().unwrap();
        let interpreter = Interpreter::new();
        let mut env = Environment::new();
        interpreter.interpret(&program, &mut env)
    }

    #[test]
    fn test_integer_expression() {
        let result = interpret_source("5;").unwrap();
        assert_eq!(result, Value::Integer(5));
    }

    #[test]
    fn test_addition() {
        let result = interpret_source("1 + 5;").unwrap();
        assert_eq!(result, Value::Integer(6));
    }

    #[test]
    fn test_assignment() {
        let result = interpret_source("x = 5; x;").unwrap();
        assert_eq!(result, Value::Integer(5));
    }

    #[test]
    fn test_function_call() {
        let source = "func add(a, b) { a + b; } add(1, 2);";
        let result = interpret_source(source).unwrap();
        assert_eq!(result, Value::Integer(3));
    }

    #[test]
    fn test_variable_declaration() {
        let result = interpret_source("x : int; x;").unwrap();
        assert_eq!(result, Value::Null);
    }
}
