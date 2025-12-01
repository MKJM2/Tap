use crate::ast::{
    BinaryOperator, Expression, LetStatement, LiteralValue, PrimaryExpression, Program,
    TopStatement, UnaryOperator,
};
use crate::environment::Environment;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
    Unit,
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum RuntimeError {
    #[error("Type error: {0}")]
    TypeError(String),
    #[error("Division by zero")]
    DivisionByZero,
}

pub struct Interpreter {
    pub env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Environment::new(),
        }
    }

    pub fn interpret(&mut self, program: &Program) -> Result<Option<Value>, RuntimeError> {
        let mut last_val = None;
        for statement in &program.statements {
            last_val = Some(self.eval_top_statement(statement)?);
        }
        Ok(last_val)
    }

    fn eval_top_statement(&mut self, statement: &TopStatement) -> Result<Value, RuntimeError> {
        match statement {
            TopStatement::Expression(expr_stmt) => self.eval_expr(&expr_stmt.expression),
            TopStatement::LetStmt(let_stmt) => self.eval_let_statement(let_stmt),
            _ => unimplemented!(),
        }
    }

    fn eval_let_statement(&mut self, let_stmt: &LetStatement) -> Result<Value, RuntimeError> {
        match let_stmt {
            LetStatement::Variable(var_binding) => {
                let value = self.eval_expr(&var_binding.value)?;
                self.env.define(var_binding.name.clone(), value);
                Ok(Value::Unit)
            }
            _ => unimplemented!(),
        }
    }

    pub fn eval_expr(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
        match expr {
            Expression::Primary(PrimaryExpression::Literal(literal, _)) => {
                self.eval_literal(literal)
            }
            Expression::Binary(binary_expr) => {
                let left = self.eval_expr(&binary_expr.left)?;
                let right = self.eval_expr(&binary_expr.right)?;
                self.apply_binary_op(left, binary_expr.operator, right)
            }
            Expression::Unary(unary_expr) => {
                let right = self.eval_expr(&unary_expr.right)?;
                self.apply_unary_op(unary_expr.operator, right)
            }
            Expression::Primary(PrimaryExpression::Identifier(name, _)) => {
                self.env.get(name).ok_or(RuntimeError::TypeError(format!(
                    "Undefined variable: {}",
                    name
                )))
            }
            _ => unimplemented!(),
        }
    }

    fn apply_unary_op(&self, op: UnaryOperator, right: Value) -> Result<Value, RuntimeError> {
        match op {
            UnaryOperator::Minus => match right {
                Value::Integer(i) => Ok(Value::Integer(-i)),
                Value::Float(f) => Ok(Value::Float(-f)),
                _ => Err(RuntimeError::TypeError(
                    "Unary minus can only be applied to integers and floats".into(),
                )),
            },
            UnaryOperator::Not => Ok(Value::Boolean(!self.is_truthy(&right))),
            UnaryOperator::Plus => match right {
                Value::Integer(i) => Ok(Value::Integer(i)),
                Value::Float(f) => Ok(Value::Float(f)),
                _ => Err(RuntimeError::TypeError(
                    "Unary plus can only be applied to integers and floats".into(),
                )),
            },
        }
    }

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Boolean(b) => *b,
            Value::Unit => false,
            _ => true,
        }
    }

    fn apply_binary_op(
        &self,
        left: Value,
        op: BinaryOperator,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => match op {
                BinaryOperator::Add => Ok(Value::Integer(l + r)),
                BinaryOperator::Subtract => Ok(Value::Integer(l - r)),
                BinaryOperator::Multiply => Ok(Value::Integer(l * r)),
                BinaryOperator::Divide => {
                    if r == 0 {
                        return Err(RuntimeError::DivisionByZero);
                    }
                    Ok(Value::Integer(l / r))
                }
                _ => Err(RuntimeError::TypeError("Invalid integer operator".into())),
            },
            (Value::Float(l), Value::Float(r)) => match op {
                BinaryOperator::Add => Ok(Value::Float(l + r)),
                BinaryOperator::Subtract => Ok(Value::Float(l - r)),
                BinaryOperator::Multiply => Ok(Value::Float(l * r)),
                BinaryOperator::Divide => Ok(Value::Float(l / r)),
                _ => Err(RuntimeError::TypeError("Invalid float operator".into())),
            },
            _ => Err(RuntimeError::TypeError(
                "Type mismatch in binary operation".into(),
            )),
        }
    }

    fn eval_literal(&self, literal: &LiteralValue) -> Result<Value, RuntimeError> {
        match literal {
            LiteralValue::Integer(i) => Ok(Value::Integer(*i)),
            LiteralValue::Float(f) => Ok(Value::Float(*f)),
            LiteralValue::String(s) => Ok(Value::String(s.clone())),
            LiteralValue::Boolean(b) => Ok(Value::Boolean(*b)),
            LiteralValue::None => Ok(Value::Unit),
        }
    }
}
