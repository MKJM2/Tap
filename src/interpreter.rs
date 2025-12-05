use crate::ast::*;
use crate::environment::Environment;
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    List(Vec<Value>),
    Record(HashMap<String, Value>),
    Function {
        name: Option<String>,
        params: Vec<Parameter>,
        body: Block,
        env: Environment,
    },
    BuiltInMethod {
        receiver: Box<Value>,
        method: String,
    },
    File {
        id: usize,      // index into interpreter's file table
        path: String,   // path to file
        mode: FileMode, // R/W/Append
        closed: bool,
    },
    Args(Args),
    Variant {
        name: String,
        data: Option<Box<Value>>,
    },
    Range {
        start: i64,
        end: i64,
        inclusive: bool,
    },
    Map(HashMap<MapKey, Value>),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FileMode {
    Read,
    Write,
    Append,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum MapKey {
    Integer(i64),
    String(String),
    Boolean(bool),
}

impl MapKey {
    pub fn from_value(value: &Value) -> Result<MapKey, RuntimeError> {
        match value {
            Value::Integer(i) => Ok(MapKey::Integer(*i)),
            Value::String(s) => Ok(MapKey::String(s.clone())),
            Value::Boolean(b) => Ok(MapKey::Boolean(*b)),
            _ => Err(RuntimeError::Type(
                "Map keys must be int, string, or bool".to_string(),
            )),
        }
    }

    pub fn to_value(&self) -> Value {
        match self {
            MapKey::Integer(i) => Value::Integer(*i),
            MapKey::String(s) => Value::String(s.clone()),
            MapKey::Boolean(b) => Value::Boolean(*b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Args {
    program: String,                  // argv[0]
    values: Vec<String>,              // positional args
    flags: HashMap<String, bool>,     // --flag, -f
    options: HashMap<String, String>, // --key=value, --key value
}

impl Args {
    pub fn empty() -> Self {
        Args {
            program: String::new(),
            values: Vec::new(),
            flags: HashMap::new(),
            options: HashMap::new(),
        }
    }
    pub fn parse(args: Vec<String>) -> Self {
        let mut parsed = Args::empty();
        if args.is_empty() {
            return parsed;
        }
        parsed.program = args[0].clone();
        let mut i = 1;
        while i < args.len() {
            let arg = &args[i];
            if arg.starts_with("--") {
                if let Some(eq_pos) = arg.find('=') {
                    let key = arg[2..eq_pos].to_string();
                    let value = arg[eq_pos + 1..].to_string();
                    parsed.options.insert(key, value);
                } else {
                    let key = arg[2..].to_string();
                    if i + 1 < args.len() && !args[i + 1].starts_with('-') {
                        parsed.options.insert(key, args[i + 1].clone());
                        i += 1;
                    } else {
                        parsed.flags.insert(key, true);
                    }
                }
            } else if arg.starts_with('-') && arg.len() > 1 {
                for ch in arg[1..].chars() {
                    parsed.flags.insert(ch.to_string(), true);
                }
            } else {
                parsed.values.push(arg.clone());
            }
            i += 1;
        }
        parsed
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum RuntimeError {
    #[error("Type error: {0}")]
    Type(String),
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Return: {0:?}")]
    Return(Value),
    #[error("Break")]
    Break,
    #[error("Continue")]
    Continue,
}

pub struct Interpreter {
    pub env: Environment,
    files: Vec<Option<std::fs::File>>, // fd table
    next_fd: usize,                    // next available fd
    args: Args,                        // parsed cmdline
}

impl Interpreter {
    pub fn new() -> Self {
        let mut interp = Interpreter {
            env: Environment::new(),
            files: vec![None, None, None], // 0,1,2 for stdin, stdout, stderr
            next_fd: 3,
            args: Args::empty(),
        };
        interp.inject_builtins();
        interp
    }

    pub fn new_with_args(args: Vec<String>) -> Self {
        let mut interp = Self::new();
        interp.args = Args::parse(args);
        interp.inject_globals();
        interp
    }

    fn inject_builtins(&mut self) {
        // Inject built-in functions as special function values
        let builtins = vec!["print", "eprint", "open", "input", "Map"];
        for name in builtins {
            self.env.define(
                name.to_string(),
                Value::Function {
                    name: Some(name.to_string()),
                    params: vec![],
                    body: Block {
                        statements: vec![],
                        final_expression: None,
                        span: Span { start: 0, end: 0 },
                    },
                    env: Environment::new(),
                },
            );
        }
    }

    fn inject_globals(&mut self) {
        self.env
            .define("args".to_string(), Value::Args(self.args.clone()));
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
            TopStatement::TypeDecl(type_decl) => {
                if let TypeConstructor::Sum(sum_ctor) = &type_decl.constructor {
                    for variant in &sum_ctor.variants {
                        if variant.ty.is_some() {
                            // Variant with data - create a constructor function
                            let variant_name = variant.name.clone();
                            let constructor = Value::Function {
                                name: Some(format!("{}Constructor", variant_name)),
                                params: vec![Parameter {
                                    name: "value".to_string(),
                                    ty: Type::Primary(TypePrimary::Named(
                                        "any".to_string(),
                                        variant.span,
                                    )),
                                    span: variant.span,
                                }],
                                body: Block {
                                    statements: vec![],
                                    final_expression: Some(Box::new(Expression::Primary(
                                        PrimaryExpression::Identifier(
                                            "value".to_string(),
                                            variant.span,
                                        ),
                                    ))),
                                    span: variant.span,
                                },
                                env: self.env.clone(),
                            };
                            self.env.define(variant_name.clone(), constructor);
                        } else {
                            // Unit variant - just a marker
                            self.env.define(
                                variant.name.clone(),
                                Value::Variant {
                                    name: variant.name.clone(),
                                    data: None,
                                },
                            );
                        }
                    }
                }
                Ok(Value::Unit)
            }
        }
    }

    fn eval_let_statement(&mut self, let_stmt: &LetStatement) -> Result<Value, RuntimeError> {
        match let_stmt {
            LetStatement::Variable(var_binding) => {
                let value = self.eval_expr(&var_binding.value)?;
                // Check if variable exists first
                if self.env.get(&var_binding.name).is_some() {
                    // Update existing variable
                    self.env.set(&var_binding.name, value);
                } else {
                    // Create new variable
                    self.env.define(var_binding.name.clone(), value);
                }
                Ok(Value::Unit)
            }
            LetStatement::Function(func_binding) => {
                let func_value = Value::Function {
                    name: Some(func_binding.name.clone()),
                    params: func_binding.params.clone(),
                    body: func_binding.body.clone(),
                    env: self.env.clone(),
                };
                self.env.define(func_binding.name.clone(), func_value);
                Ok(Value::Unit)
            }
        }
    }

    fn eval_statement(&mut self, stmt: &Statement) -> Result<Value, RuntimeError> {
        match stmt {
            Statement::Let(let_stmt) => self.eval_let_statement(let_stmt),
            Statement::Expression(expr_stmt) => self.eval_expr(&expr_stmt.expression),
            Statement::Return(expr_opt, _) => {
                let value = if let Some(expr) = expr_opt {
                    self.eval_expr(expr)?
                } else {
                    Value::Unit
                };
                Err(RuntimeError::Return(value))
            }
            Statement::Break(_) => Err(RuntimeError::Break),
            Statement::Continue(_) => Err(RuntimeError::Continue),
        }
    }

    pub fn eval_expr(&mut self, expr: &Expression) -> Result<Value, RuntimeError> {
        match expr {
            Expression::Primary(primary) => self.eval_primary(primary),
            Expression::Binary(binary_expr) => {
                // Handle assignment operators
                if matches!(
                    binary_expr.operator,
                    BinaryOperator::Assign
                        | BinaryOperator::AddAssign
                        | BinaryOperator::SubtractAssign
                        | BinaryOperator::MultiplyAssign
                        | BinaryOperator::DivideAssign
                        | BinaryOperator::ModuloAssign
                ) {
                    if let Expression::Primary(PrimaryExpression::Identifier(name, _)) =
                        &*binary_expr.left
                    {
                        // Evaluate and set
                        if binary_expr.operator == BinaryOperator::Assign {
                            let value = self.eval_expr(&binary_expr.right)?;
                            self.env.set(name, value);
                            return Ok(Value::Unit);
                        }

                        let current_val = self
                            .env
                            .get(name)
                            .ok_or(RuntimeError::Type(format!("Undefined variable: {}", name)))?;
                        let right_val = self.eval_expr(&binary_expr.right)?;

                        let base_op = match binary_expr.operator {
                            BinaryOperator::AddAssign => BinaryOperator::Add,
                            BinaryOperator::SubtractAssign => BinaryOperator::Subtract,
                            BinaryOperator::MultiplyAssign => BinaryOperator::Multiply,
                            BinaryOperator::DivideAssign => BinaryOperator::Divide,
                            BinaryOperator::ModuloAssign => BinaryOperator::Modulo,
                            _ => unreachable!(),
                        };

                        let new_val = self.apply_binary_op(current_val, base_op, right_val)?;
                        self.env.set(name, new_val);
                        return Ok(Value::Unit);
                    }

                    // Handle field assignment (record.field = value)
                    if let Expression::Postfix(postfix_expr) = &*binary_expr.left {
                        if let Expression::Primary(PrimaryExpression::Identifier(var_name, _)) =
                            &*postfix_expr.primary
                        {
                            // Handle nested list/field assignment
                            if !postfix_expr.operators.is_empty() {
                                let value = self.eval_expr(&binary_expr.right)?;
                                let mut root = self.env.get(var_name).ok_or(RuntimeError::Type(
                                    format!("Undefined variable: {}", var_name),
                                ))?;

                                root =
                                    self.update_nested_value(root, &postfix_expr.operators, value)?;
                                self.env.set(var_name, root);
                                return Ok(Value::Unit);
                            }
                        }
                    }
                }

                // TODO: if binary_op is an AND,
                // we should short-circuit evaluation & not evaluate the right-side,
                // in case the right-hand side has side-effects
                let left = self.eval_expr(&binary_expr.left)?;
                let right = self.eval_expr(&binary_expr.right)?;
                self.apply_binary_op(left, binary_expr.operator, right)
            }
            Expression::Unary(unary_expr) => {
                let right = self.eval_expr(&unary_expr.right)?;
                self.apply_unary_op(unary_expr.operator, right)
            }
            Expression::If(if_expr) => self.eval_if_expression(if_expr),
            Expression::While(while_expr) => self.eval_while_expression(while_expr),
            Expression::For(for_expr) => self.eval_for_expression(for_expr),
            Expression::Match(match_expr) => self.eval_match_expression(match_expr),
            Expression::Lambda(lambda_expr) => self.eval_lambda_expression(lambda_expr),
            Expression::Block(block) => {
                self.env.push_scope();
                let result = self.eval_block(block);
                self.env.pop_scope();
                result
            }
            Expression::Postfix(postfix_expr) => self.eval_postfix_expression(postfix_expr),
            Expression::Range(range_expr) => self.eval_range_expression(range_expr),
        }
    }

    fn update_nested_value(
        &mut self,
        current: Value,
        operators: &[PostfixOperator],
        new_value: Value,
    ) -> Result<Value, RuntimeError> {
        if operators.is_empty() {
            return Ok(new_value);
        }

        match &operators[0] {
            PostfixOperator::ListAccess { index, .. } => {
                if let Value::List(mut elements) = current {
                    let idx_val = self.eval_expr(index)?;
                    if let Value::Integer(idx) = idx_val {
                        if idx < 0 || idx as usize >= elements.len() {
                            return Err(RuntimeError::Type(format!("Index {} out of bounds", idx)));
                        }

                        let idx = idx as usize;
                        // Recursively update nested value
                        elements[idx] = self.update_nested_value(
                            elements[idx].clone(),
                            &operators[1..],
                            new_value,
                        )?;

                        return Ok(Value::List(elements));
                    }
                    Err(RuntimeError::Type("List index must be integer".into()))
                } else {
                    Err(RuntimeError::Type("Cannot index non-list value".into()))
                }
            }
            PostfixOperator::FieldAccess { name, .. } => {
                if let Value::Record(mut fields) = current {
                    // Recursively update nested value
                    let old_value = fields.get(name).cloned().unwrap_or(Value::Unit);
                    fields.insert(
                        name.clone(),
                        self.update_nested_value(old_value, &operators[1..], new_value)?,
                    );
                    Ok(Value::Record(fields))
                } else {
                    Err(RuntimeError::Type(
                        "Cannot access field on non-record".into(),
                    ))
                }
            }
            _ => Err(RuntimeError::Type("Unsupported nested assignment".into())),
        }
    }

    fn eval_range_expression(
        &mut self,
        range_expr: &RangeExpression,
    ) -> Result<Value, RuntimeError> {
        let start_val = self.eval_expr(&range_expr.start)?;
        let end_val = self.eval_expr(&range_expr.end)?;

        match (start_val, end_val) {
            (Value::Integer(start), Value::Integer(end)) => Ok(Value::Range {
                start,
                end,
                inclusive: range_expr.inclusive,
            }),
            (Value::Integer(_), _) => Err(RuntimeError::Type(
                "Range end bound must be an integer".to_string(),
            )),
            (_, Value::Integer(_)) => Err(RuntimeError::Type(
                "Range start bound must be an integer".to_string(),
            )),
            _ => Err(RuntimeError::Type(
                "Range bounds must be integers".to_string(),
            )),
        }
    }

    fn eval_primary(&mut self, primary: &PrimaryExpression) -> Result<Value, RuntimeError> {
        match primary {
            PrimaryExpression::Literal(literal, _) => self.eval_literal(literal),
            PrimaryExpression::Identifier(name, _) => self
                .env
                .get(name)
                .ok_or(RuntimeError::Type(format!("Undefined variable: {}", name))),
            PrimaryExpression::Parenthesized(expr, _) => self.eval_expr(expr),
            PrimaryExpression::List(list_literal) => {
                let mut elements = Vec::new();
                for elem_expr in &list_literal.elements {
                    elements.push(self.eval_expr(elem_expr)?);
                }
                Ok(Value::List(elements))
            }
            PrimaryExpression::Record(record_literal) => self.eval_record_literal(record_literal),
            PrimaryExpression::This(_) => self.env.get("this").ok_or(RuntimeError::Type(
                "Cannot use 'this' outside of a method".to_string(),
            )),
        }
    }

    fn eval_if_expression(&mut self, if_expr: &IfExpression) -> Result<Value, RuntimeError> {
        let condition = self.eval_expr(&if_expr.condition)?;
        if self.is_truthy(&condition) {
            self.env.push_scope();
            let result = self.eval_block(&if_expr.then_branch);
            self.env.pop_scope();
            result
        } else if let Some(else_branch) = &if_expr.else_branch {
            self.eval_expr(else_branch)
        } else {
            Ok(Value::Unit)
        }
    }

    fn eval_while_expression(
        &mut self,
        while_expr: &WhileExpression,
    ) -> Result<Value, RuntimeError> {
        loop {
            let condition = self.eval_expr(&while_expr.condition)?;
            if !self.is_truthy(&condition) {
                break;
            }
            // Don't push scope - the block manages its own scope
            match self.eval_block(&while_expr.body) {
                Err(RuntimeError::Break) => break,
                Err(RuntimeError::Continue) => continue,
                Err(e) => return Err(e),
                Ok(_) => {}
            }
        }
        Ok(Value::Unit)
    }

    fn eval_for_expression(&mut self, for_expr: &ForExpression) -> Result<Value, RuntimeError> {
        let iterable = self.eval_expr(&for_expr.iterable)?;
        match iterable {
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                let actual_end = if inclusive { end + 1 } else { end };

                for i in start..actual_end {
                    self.bind_pattern(&for_expr.pattern, Value::Integer(i))?;
                    match self.eval_block(&for_expr.body) {
                        Err(RuntimeError::Break) => break,
                        Err(RuntimeError::Continue) => continue,
                        Err(e) => return Err(e),
                        Ok(_) => {}
                    }
                }
                Ok(Value::Unit)
            }
            Value::List(elements) => {
                for element in elements {
                    // Don't push extra scope - bind pattern in current scope
                    self.bind_pattern(&for_expr.pattern, element)?;
                    match self.eval_block(&for_expr.body) {
                        Err(RuntimeError::Break) => break,
                        Err(RuntimeError::Continue) => continue,
                        Err(e) => return Err(e),
                        Ok(_) => {}
                    }
                }
                Ok(Value::Unit)
            }
            _ => Err(RuntimeError::Type(
                "For loop requires an iterable value".to_string(),
            )),
        }
    }

    fn eval_match_expression(
        &mut self,
        match_expr: &MatchExpression,
    ) -> Result<Value, RuntimeError> {
        let value = self.eval_expr(&match_expr.value)?;

        for arm in &match_expr.arms {
            if self.pattern_matches(&arm.pattern, &value)? {
                self.env.push_scope();
                self.bind_pattern(&arm.pattern, value.clone())?;

                let result = match &arm.body {
                    ExpressionOrBlock::Expression(expr) => self.eval_expr(expr),
                    ExpressionOrBlock::Block(block) => self.eval_block(block),
                };

                self.env.pop_scope();
                return result;
            }
        }

        Err(RuntimeError::Type(
            "No matching pattern in match expression".to_string(),
        ))
    }

    fn eval_lambda_expression(
        &mut self,
        lambda_expr: &LambdaExpression,
    ) -> Result<Value, RuntimeError> {
        let body = match &lambda_expr.body {
            ExpressionOrBlock::Block(block) => block.clone(),
            ExpressionOrBlock::Expression(expr) => Block {
                statements: vec![],
                final_expression: Some(expr.clone()),
                span: expr.span(),
            },
        };

        Ok(Value::Function {
            name: None,
            params: lambda_expr.params.clone(),
            body,
            env: self.env.clone(),
        })
    }

    fn eval_block(&mut self, block: &Block) -> Result<Value, RuntimeError> {
        // Don't push scope here - caller handles it
        for stmt in &block.statements {
            self.eval_statement(stmt)?;
        }

        if let Some(final_expr) = &block.final_expression {
            self.eval_expr(final_expr)
        } else {
            Ok(Value::Unit)
        }
    }

    fn eval_postfix_expression(
        &mut self,
        postfix_expr: &PostfixExpression,
    ) -> Result<Value, RuntimeError> {
        let mut value = self.eval_expr(&postfix_expr.primary)?;

        // Track if the primary is a simple identifier for mutation tracking
        let root_var_name = if let Expression::Primary(PrimaryExpression::Identifier(name, _)) =
            &*postfix_expr.primary
        {
            Some(name.clone())
        } else {
            None
        };

        for op in postfix_expr.operators.iter() {
            // Pass var_name to ALL operations for chaining support
            let var_name_for_mutation = root_var_name.as_deref();

            value = match op {
                PostfixOperator::Call { args, .. } => {
                    self.eval_function_call(value, args, var_name_for_mutation)?
                }
                PostfixOperator::FieldAccess { name, .. } => self.eval_field_access(value, name)?,
                PostfixOperator::ListAccess { index, .. } => self.eval_list_access(value, index)?,
                PostfixOperator::TypePath { .. } => unimplemented!(),
            };
        }

        Ok(value)
    }

    fn eval_function_call(
        &mut self,
        func_value: Value,
        args: &[Expression],
        var_name: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        // Check if it's an identifier being called as a function (for built-ins)
        if let Value::Function {
            name: Some(func_name),
            ..
        } = &func_value
        {
            // Check for built-in functions
            match func_name.as_str() {
                "print" => {
                    if args.len() != 1 {
                        return Err(RuntimeError::Type("print expects 1 argument".into()));
                    }
                    let value = self.eval_expr(&args[0])?;
                    println!("{}", self.value_to_display_string(&value));
                    return Ok(Value::Unit);
                }
                "eprint" => {
                    if args.len() != 1 {
                        return Err(RuntimeError::Type("eprint expects 1 argument".into()));
                    }
                    let value = self.eval_expr(&args[0])?;
                    eprintln!("{}", self.value_to_display_string(&value));
                    return Ok(Value::Unit);
                }
                "open" => {
                    if args.len() != 2 {
                        return Err(RuntimeError::Type("open expects 2 arguments".into()));
                    }
                    let path = self.eval_expr(&args[0])?;
                    let mode = self.eval_expr(&args[1])?;
                    if let (Value::String(p), Value::String(m)) = (path, mode) {
                        return self.open_file(p, m);
                    }
                    return Err(RuntimeError::Type("open arguments must be strings".into()));
                }
                "input" => {
                    use std::io::{self, Write};
                    if args.len() > 1 {
                        return Err(RuntimeError::Type("input expects 0 or 1 argument".into()));
                    }
                    if args.len() == 1 {
                        let prompt = self.eval_expr(&args[0])?;
                        print!("{}", self.value_to_display_string(&prompt));
                        io::stdout().flush().ok();
                    }
                    let mut line = String::new();
                    io::stdin()
                        .read_line(&mut line)
                        .map_err(|_| RuntimeError::Type("Failed to read from stdin".into()))?;
                    return Ok(Value::String(line.trim_end_matches('\n').to_string()));
                }
                "Map" => {
                    if !args.is_empty() {
                        return Err(RuntimeError::Type("Map() takes no arguments".into()));
                    }
                    return Ok(Value::Map(HashMap::new()));
                }
                _ => {} // Not a built-in, continue with regular function call
            }
        }
        match func_value {
            Value::BuiltInMethod { receiver, method } => {
                self.eval_builtin_method(*receiver, &method, args, var_name)
            }
            Value::Function {
                name,
                params,
                body,
                env,
            } => {
                // Check if this is a variant constructor
                if let Some(func_name) = &name {
                    if func_name.ends_with("Constructor") {
                        let variant_name = func_name.trim_end_matches("Constructor");
                        if args.len() == 1 {
                            let data = self.eval_expr(&args[0])?;
                            return Ok(Value::Variant {
                                name: variant_name.to_string(),
                                data: Some(Box::new(data)),
                            });
                        }
                    }
                }

                // Regular function call
                if params.len() != args.len() {
                    return Err(RuntimeError::Type(format!(
                        "Function expects {} arguments, got {}",
                        params.len(),
                        args.len()
                    )));
                }

                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.eval_expr(arg)?);
                }

                let saved_env = std::mem::replace(&mut self.env, env.clone());
                self.env.push_scope();

                if let Some(func_name) = &name {
                    self.env.define(
                        func_name.clone(),
                        Value::Function {
                            name: name.clone(),
                            params: params.clone(),
                            body: body.clone(),
                            env: self.env.clone(),
                        },
                    );
                }

                for (param, arg_value) in params.iter().zip(arg_values.iter()) {
                    self.env.define(param.name.clone(), arg_value.clone());
                }

                let result = match self.eval_block(&body) {
                    Ok(val) => Ok(val),
                    Err(RuntimeError::Return(val)) => Ok(val),
                    Err(e) => Err(e),
                };

                self.env.pop_scope();
                self.env = saved_env;

                result
            }
            _ => Err(RuntimeError::Type(format!(
                "Cannot call non-function value {}",
                self.value_to_display_string(&func_value)
            ))),
        }
    }

    fn eval_builtin_method(
        &mut self,
        receiver: Value,
        method: &str,
        args: &[Expression],
        var_name: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        // Helper macro for in-place mutations
        macro_rules! mutate_in_place {
            ($new_val:expr) => {{
                let val = $new_val;
                if let Some(name) = var_name {
                    self.env.set(name, val.clone());
                }
                return Ok(val);
            }};
        }
        match (&receiver, method) {
            // ==================== MAP METHODS ====================
            (Value::Map(_), "insert") => {
                if args.len() != 2 {
                    return Err(RuntimeError::Type("insert expects 2 arguments".into()));
                }
                let key_val = self.eval_expr(&args[0])?;
                let value_val = self.eval_expr(&args[1])?;

                let key = MapKey::from_value(&key_val)?;

                let mut map = if let Value::Map(m) = receiver {
                    m
                } else {
                    unreachable!()
                };

                map.insert(key, value_val);
                mutate_in_place!(Value::Map(map));
            }

            (Value::Map(map), "get") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("get expects 1 argument".into()));
                }
                let key_val = self.eval_expr(&args[0])?;
                let key = MapKey::from_value(&key_val)?;

                map.get(&key).cloned().ok_or(RuntimeError::Type(format!(
                    "Key {:?} not found in map",
                    key
                )))
            }

            (Value::Map(map), "has") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("has expects 1 argument".into()));
                }
                let key_val = self.eval_expr(&args[0])?;
                let key = MapKey::from_value(&key_val)?;
                Ok(Value::Boolean(map.contains_key(&key)))
            }

            (Value::Map(map), "contains") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("contains expects 1 argument".into()));
                }
                let key_val = self.eval_expr(&args[0])?;
                let key = MapKey::from_value(&key_val)?;
                Ok(Value::Boolean(map.contains_key(&key)))
            }

            (Value::Map(_), "remove") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("remove expects 1 argument".into()));
                }
                let key_val = self.eval_expr(&args[0])?;
                let key = MapKey::from_value(&key_val)?;

                let mut map = if let Value::Map(m) = receiver {
                    m
                } else {
                    unreachable!()
                };

                let removed = map.remove(&key).ok_or(RuntimeError::Type(format!(
                    "Key {:?} not found in map",
                    key
                )))?;

                if let Some(name) = var_name {
                    self.env.set(name, Value::Map(map));
                }
                Ok(removed)
            }

            (Value::Map(map), "length") | (Value::Map(map), "size") => {
                Ok(Value::Integer(map.len() as i64))
            }

            (Value::Map(map), "is_empty") => Ok(Value::Boolean(map.is_empty())),

            (Value::Map(_), "clear") => {
                let map = HashMap::new();
                mutate_in_place!(Value::Map(map));
            }

            (Value::Map(map), "keys") => {
                let keys: Vec<Value> = map.keys().map(|k| k.to_value()).collect();
                Ok(Value::List(keys))
            }

            (Value::Map(map), "values") => {
                let values: Vec<Value> = map.values().cloned().collect();
                Ok(Value::List(values))
            }

            (Value::Map(map), "entries") => {
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

            // ==================== LIST METHODS (MUTATING) ====================
            (Value::List(_), "push") | (Value::List(_), "append") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type(
                        format!("{} expects 1 argument", method).into(),
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                let mut list = if let Value::List(l) = receiver {
                    l
                } else {
                    unreachable!()
                };
                list.push(value);
                mutate_in_place!(Value::List(list));
            }

            (Value::List(_), "pop") => {
                let mut list = if let Value::List(l) = receiver {
                    l
                } else {
                    unreachable!()
                };
                if list.is_empty() {
                    return Err(RuntimeError::Type("Cannot pop from empty list".into()));
                }
                let popped = list.pop().unwrap();
                if let Some(name) = var_name {
                    self.env.set(name, Value::List(list));
                }
                Ok(popped)
            }

            (Value::List(_), "remove") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("remove expects 1 argument".into()));
                }
                let idx = self.eval_expr(&args[0])?;
                if let Value::Integer(i) = idx {
                    let mut list = if let Value::List(l) = receiver {
                        l
                    } else {
                        unreachable!()
                    };
                    if i < 0 || i as usize >= list.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", i)));
                    }
                    let removed = list.remove(i as usize);
                    if let Some(name) = var_name {
                        self.env.set(name, Value::List(list));
                    }
                    Ok(removed)
                } else {
                    Err(RuntimeError::Type("remove index must be integer".into()))
                }
            }

            (Value::List(_), "insert") => {
                if args.len() != 2 {
                    return Err(RuntimeError::Type("insert expects 2 arguments".into()));
                }
                let idx = self.eval_expr(&args[0])?;
                let value = self.eval_expr(&args[1])?;
                if let Value::Integer(i) = idx {
                    let mut list = if let Value::List(l) = receiver {
                        l
                    } else {
                        unreachable!()
                    };
                    if i < 0 || i as usize > list.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", i)));
                    }
                    list.insert(i as usize, value);
                    mutate_in_place!(Value::List(list));
                } else {
                    Err(RuntimeError::Type("insert index must be integer".into()))
                }
            }

            (Value::List(_), "reverse") => {
                let mut list = if let Value::List(l) = receiver {
                    l
                } else {
                    unreachable!()
                };
                list.reverse();
                mutate_in_place!(Value::List(list));
            }

            (Value::List(_), "sort") => {
                let mut list = if let Value::List(l) = receiver {
                    l
                } else {
                    unreachable!()
                };

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

                mutate_in_place!(Value::List(list));
            }

            // ==================== STRING METHODS ====================

            // String length
            (Value::String(s), "length") => Ok(Value::Integer(s.len() as i64)),

            // String splitting
            (Value::String(s), "split") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("split expects 1 argument".into()));
                }
                let delim = self.eval_expr(&args[0])?;
                if let Value::String(d) = delim {
                    let parts: Vec<Value> = s
                        .split(d.as_str())
                        .map(|p| Value::String(p.to_string()))
                        .collect();
                    Ok(Value::List(parts))
                } else {
                    Err(RuntimeError::Type("split delimiter must be string".into()))
                }
            }

            // Parse string to integer
            (Value::String(s), "parse_int") => s
                .trim()
                .parse::<i64>()
                .map(Value::Integer)
                .map_err(|_| RuntimeError::Type(format!("Cannot parse '{}' as integer", s))),

            // Parse string to float
            (Value::String(s), "parse_float") => s
                .trim()
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| RuntimeError::Type(format!("Cannot parse '{}' as float", s))),

            // Trim whitespace
            (Value::String(s), "trim") => Ok(Value::String(s.trim().to_string())),

            // Trim start
            (Value::String(s), "trim_start") => Ok(Value::String(s.trim_start().to_string())),

            // Trim end
            (Value::String(s), "trim_end") => Ok(Value::String(s.trim_end().to_string())),

            // Check if string contains substring
            (Value::String(s), "contains") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("contains expects 1 argument".into()));
                }
                let needle = self.eval_expr(&args[0])?;
                if let Value::String(n) = needle {
                    Ok(Value::Boolean(s.contains(n.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "contains argument must be string".into(),
                    ))
                }
            }

            // Check if string starts with prefix
            (Value::String(s), "starts_with") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("starts_with expects 1 argument".into()));
                }
                let prefix = self.eval_expr(&args[0])?;
                if let Value::String(p) = prefix {
                    Ok(Value::Boolean(s.starts_with(p.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "starts_with argument must be string".into(),
                    ))
                }
            }

            // Check if string ends with suffix
            (Value::String(s), "ends_with") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("ends_with expects 1 argument".into()));
                }
                let suffix = self.eval_expr(&args[0])?;
                if let Value::String(suf) = suffix {
                    Ok(Value::Boolean(s.ends_with(suf.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "ends_with argument must be string".into(),
                    ))
                }
            }

            // Replace substring
            (Value::String(s), "replace") => {
                if args.len() != 2 {
                    return Err(RuntimeError::Type("replace expects 2 arguments".into()));
                }
                let from = self.eval_expr(&args[0])?;
                let to = self.eval_expr(&args[1])?;
                if let (Value::String(f), Value::String(t)) = (from, to) {
                    Ok(Value::String(s.replace(f.as_str(), t.as_str())))
                } else {
                    Err(RuntimeError::Type(
                        "replace arguments must be strings".into(),
                    ))
                }
            }

            // Convert to lowercase
            (Value::String(s), "to_lower") => Ok(Value::String(s.to_lowercase())),

            // Convert to uppercase
            (Value::String(s), "to_upper") => Ok(Value::String(s.to_uppercase())),

            // Get character at index
            (Value::String(s), "char_at") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("char_at expects 1 argument".into()));
                }
                let idx = self.eval_expr(&args[0])?;
                if let Value::Integer(i) = idx {
                    if i < 0 || i as usize >= s.len() {
                        return Err(RuntimeError::Type(format!("Index {} out of bounds", i)));
                    }
                    let ch = s.chars().nth(i as usize).unwrap();
                    Ok(Value::String(ch.to_string()))
                } else {
                    Err(RuntimeError::Type("char_at index must be integer".into()))
                }
            }

            // Get chars as list
            (Value::String(s), "chars") => {
                let chars: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();
                Ok(Value::List(chars))
            }

            // Find first occurrence of substring
            (Value::String(s), "index_of") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("index_of expects 1 argument".into()));
                }
                let needle = self.eval_expr(&args[0])?;
                if let Value::String(n) = needle {
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

            // Substring (already exists but keeping for completeness)
            (Value::String(s), "substring") => {
                if args.len() != 2 {
                    return Err(RuntimeError::Type("substring expects 2 arguments".into()));
                }
                let start = self.eval_expr(&args[0])?;
                let len = self.eval_expr(&args[1])?;
                match (start, len) {
                    (Value::Integer(start), Value::Integer(len)) => {
                        let start = start as usize;
                        let len = len as usize;
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

            // ==================== LIST METHODS (NON-MUTATING) ====================

            // List length
            (Value::List(elements), "length") => Ok(Value::Integer(elements.len() as i64)),

            // Check if list contains value
            (Value::List(elements), "contains") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("contains expects 1 argument".into()));
                }
                let target = self.eval_expr(&args[0])?;
                Ok(Value::Boolean(elements.contains(&target)))
            }

            // Find index of value
            (Value::List(elements), "index_of") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("index_of expects 1 argument".into()));
                }
                let target = self.eval_expr(&args[0])?;
                match elements.iter().position(|v| v == &target) {
                    Some(idx) => Ok(Value::Integer(idx as i64)),
                    None => Ok(Value::Integer(-1)),
                }
            }

            // Slice list
            (Value::List(elements), "slice") => {
                if args.len() != 2 {
                    return Err(RuntimeError::Type("slice expects 2 arguments".into()));
                }
                let start = self.eval_expr(&args[0])?;
                let end = self.eval_expr(&args[1])?;
                match (start, end) {
                    (Value::Integer(s), Value::Integer(e)) => {
                        let start = s.max(0) as usize;
                        let end = (e.max(0) as usize).min(elements.len());
                        if start <= end && start <= elements.len() {
                            Ok(Value::List(elements[start..end].to_vec()))
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

            // Join list of strings
            (Value::List(elements), "join") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("join expects 1 argument".into()));
                }
                let separator = self.eval_expr(&args[0])?;
                if let Value::String(sep) = separator {
                    let strings: Result<Vec<String>, _> = elements
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
                        Ok(strs) => Ok(Value::String(strs.join(&sep))),
                        Err(e) => Err(e),
                    }
                } else {
                    Err(RuntimeError::Type("join separator must be string".into()))
                }
            }

            // Map over list
            (Value::List(elements), "map") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type(
                        "map expects 1 argument (function)".into(),
                    ));
                }
                let func = self.eval_expr(&args[0])?;
                let mut results = Vec::new();

                for elem in elements {
                    let result = self.eval_function_call(
                        func.clone(),
                        &[Expression::Primary(PrimaryExpression::Literal(
                            match elem {
                                Value::Integer(i) => LiteralValue::Integer(*i),
                                Value::Float(f) => LiteralValue::Float(*f),
                                Value::String(s) => LiteralValue::String(s.clone()),
                                Value::Boolean(b) => LiteralValue::Boolean(*b),
                                Value::Unit => LiteralValue::None,
                                _ => {
                                    return Err(RuntimeError::Type(
                                        "Cannot map complex types".into(),
                                    ));
                                }
                            },
                            crate::ast::Span { start: 0, end: 0 },
                        ))],
                        var_name,
                    )?;
                    results.push(result);
                }

                Ok(Value::List(results))
            }

            // Filter list
            (Value::List(elements), "filter") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type(
                        "filter expects 1 argument (function)".into(),
                    ));
                }
                let func = self.eval_expr(&args[0])?;
                let mut results = Vec::new();

                for elem in elements {
                    let temp_expr = Expression::Primary(PrimaryExpression::Literal(
                        match elem {
                            Value::Integer(i) => LiteralValue::Integer(*i),
                            Value::Float(f) => LiteralValue::Float(*f),
                            Value::String(s) => LiteralValue::String(s.clone()),
                            Value::Boolean(b) => LiteralValue::Boolean(*b),
                            Value::Unit => LiteralValue::None,
                            _ => {
                                return Err(RuntimeError::Type(
                                    "Cannot filter complex types".into(),
                                ));
                            }
                        },
                        crate::ast::Span { start: 0, end: 0 },
                    ));

                    let keep = self.eval_function_call(func.clone(), &[temp_expr], var_name)?;
                    if let Value::Boolean(true) = keep {
                        results.push(elem.clone());
                    } else if !matches!(keep, Value::Boolean(_)) {
                        return Err(RuntimeError::Type(
                            "filter predicate must return boolean".into(),
                        ));
                    }
                }

                Ok(Value::List(results))
            }

            // Get first element
            (Value::List(elements), "first") => elements
                .first()
                .cloned()
                .ok_or(RuntimeError::Type("Cannot get first of empty list".into())),

            // Get last element
            (Value::List(elements), "last") => elements
                .last()
                .cloned()
                .ok_or(RuntimeError::Type("Cannot get last of empty list".into())),

            // Check if list is empty
            (Value::List(elements), "is_empty") => Ok(Value::Boolean(elements.is_empty())),

            // ==================== INTEGER METHODS ====================

            // Convert integer to float
            (Value::Integer(i), "to_float") => Ok(Value::Float(*i as f64)),

            // Convert integer to string
            (Value::Integer(i), "to_string") => Ok(Value::String(i.to_string())),

            // Absolute value
            (Value::Integer(i), "abs") => Ok(Value::Integer(i.abs())),

            // Power
            (Value::Integer(i), "pow") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("pow expects 1 argument".into()));
                }
                let exp = self.eval_expr(&args[0])?;
                if let Value::Integer(e) = exp {
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

            // ==================== FLOAT METHODS ====================

            // Convert float to string
            (Value::Float(f), "to_string") => Ok(Value::String(f.to_string())),

            // Convert float to integer (truncate)
            (Value::Float(f), "to_int") => Ok(Value::Integer(*f as i64)),

            // Absolute value
            (Value::Float(f), "abs") => Ok(Value::Float(f.abs())),

            // Floor
            (Value::Float(f), "floor") => Ok(Value::Float(f.floor())),

            // Ceiling
            (Value::Float(f), "ceil") => Ok(Value::Float(f.ceil())),

            // Round
            (Value::Float(f), "round") => Ok(Value::Float(f.round())),

            // Square root
            (Value::Float(f), "sqrt") => Ok(Value::Float(f.sqrt())),

            // Power
            (Value::Float(f), "pow") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("pow expects 1 argument".into()));
                }
                let exp = self.eval_expr(&args[0])?;
                match exp {
                    Value::Float(e) => Ok(Value::Float(f.powf(e))),
                    Value::Integer(e) => Ok(Value::Float(f.powi(e as i32))),
                    _ => Err(RuntimeError::Type("pow exponent must be number".into())),
                }
            }

            // ==================== BOOLEAN METHODS ====================

            // Convert boolean to string
            (Value::Boolean(b), "to_string") => Ok(Value::String(b.to_string())),

            // ==================== FILE METHODS ====================
            (Value::File { id, closed, .. }, "read") => {
                if *closed {
                    return Err(RuntimeError::Type("Cannot read from closed file".into()));
                }
                self.file_read(*id)
            }

            (Value::File { id, closed, .. }, "read_lines") => {
                if *closed {
                    return Err(RuntimeError::Type("Cannot read from closed file".into()));
                }
                self.file_read_lines(*id)
            }

            (Value::File { id, closed, .. }, "write") => {
                if *closed {
                    return Err(RuntimeError::Type("Cannot write to closed file".into()));
                }
                if args.len() != 1 {
                    return Err(RuntimeError::Type("write expects 1 argument".into()));
                }
                let text = self.eval_expr(&args[0])?;
                self.file_write(*id, &self.value_to_display_string(&text))
            }

            (Value::File { id, closed, .. }, "write_line") => {
                if *closed {
                    return Err(RuntimeError::Type("Cannot write to closed file".into()));
                }
                if args.len() != 1 {
                    return Err(RuntimeError::Type("write_line expects 1 argument".into()));
                }
                let text = self.eval_expr(&args[0])?;
                self.file_write(*id, &format!("{}\n", self.value_to_display_string(&text)))
            }

            (Value::File { id, .. }, "close") => self.file_close(*id),

            (Value::File { closed, .. }, "is_closed") => Ok(Value::Boolean(*closed)),

            // ==================== ARGS METHODS ====================
            (Value::Args(args_obj), "program") => Ok(Value::String(args_obj.program.clone())),

            (Value::Args(args_obj), "values") => Ok(Value::List(
                args_obj
                    .values
                    .iter()
                    .map(|s| Value::String(s.clone()))
                    .collect(),
            )),

            (Value::Args(args_obj), "length") => Ok(Value::Integer(args_obj.values.len() as i64)),

            (Value::Args(args_obj), "get") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("args.get expects 1 argument".into()));
                }
                let idx = self.eval_expr(&args[0])?;
                if let Value::Integer(i) = idx {
                    if i < 0 || i as usize >= args_obj.values.len() {
                        return Ok(Value::Unit);
                    }
                    Ok(Value::String(args_obj.values[i as usize].clone()))
                } else {
                    Err(RuntimeError::Type("args.get index must be integer".into()))
                }
            }

            (Value::Args(args_obj), "has") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type("args.has expects 1 argument".into()));
                }
                let flag = self.eval_expr(&args[0])?;
                if let Value::String(f) = flag {
                    Ok(Value::Boolean(args_obj.flags.contains_key(&f)))
                } else {
                    Err(RuntimeError::Type(
                        "args.has argument must be string".into(),
                    ))
                }
            }

            (Value::Args(args_obj), "get_option") => {
                if args.len() != 1 {
                    return Err(RuntimeError::Type(
                        "args.get_option expects 1 argument".into(),
                    ));
                }
                let key = self.eval_expr(&args[0])?;
                if let Value::String(k) = key {
                    match args_obj.options.get(&k) {
                        Some(v) => Ok(Value::String(v.clone())),
                        None => Ok(Value::Unit),
                    }
                } else {
                    Err(RuntimeError::Type(
                        "args.get_option argument must be string".into(),
                    ))
                }
            }

            // ==================== FALLBACK ====================
            _ => Err(RuntimeError::Type(format!(
                "Unknown method '{}' for type {:?}",
                method,
                std::mem::discriminant(&receiver)
            ))),
        }
    }

    fn eval_field_access(&mut self, value: Value, field_name: &str) -> Result<Value, RuntimeError> {
        // Check for built-in methods
        match &value {
            Value::Map(_)
                if matches!(
                    field_name,
                    "insert"
                        | "get"
                        | "has"
                        | "contains"
                        | "remove"
                        | "delete"
                        | "length"
                        | "size"
                        | "is_empty"
                        | "clear"
                        | "keys"
                        | "values"
                        | "entries"
                ) =>
            {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            Value::List(_)
                if matches!(
                    field_name,
                    "length"
                        | "push"
                        | "append"
                        | "pop"
                        | "remove"
                        | "insert"
                        | "reverse"
                        | "sort"
                        | "contains"
                        | "index_of"
                        | "slice"
                        | "join"
                        | "map"
                        | "filter"
                        | "first"
                        | "last"
                        | "is_empty"
                ) =>
            {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            Value::String(_)
                if matches!(
                    field_name,
                    "length"
                        | "substring"
                        | "split"
                        | "parse_int"
                        | "parse_float"
                        | "trim"
                        | "trim_start"
                        | "trim_end"
                        | "contains"
                        | "starts_with"
                        | "ends_with"
                        | "replace"
                        | "to_lower"
                        | "to_upper"
                        | "char_at"
                        | "chars"
                        | "index_of"
                ) =>
            {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            Value::Integer(_) if matches!(field_name, "to_float" | "to_string" | "abs" | "pow") => {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            Value::Float(_)
                if matches!(
                    field_name,
                    "to_string" | "to_int" | "abs" | "floor" | "ceil" | "round" | "sqrt" | "pow"
                ) =>
            {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            Value::Boolean(_) if field_name == "to_string" => {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            Value::File {
                id: _,
                path: _,
                mode: _,
                closed: _,
            } if matches!(
                field_name,
                "read" | "read_lines" | "write" | "write_line" | "close" | "is_closed"
            ) =>
            {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            Value::Args(_)
                if matches!(
                    field_name,
                    "program" | "values" | "length" | "get" | "has" | "get_option"
                ) =>
            {
                return Ok(Value::BuiltInMethod {
                    receiver: Box::new(value),
                    method: field_name.to_string(),
                });
            }
            _ => {}
        }

        // Handle record fields
        match value {
            Value::Record(fields) => {
                fields
                    .get(field_name)
                    .cloned()
                    .ok_or(RuntimeError::Type(format!(
                        "Field '{}' not found",
                        field_name
                    )))
            }
            _ => Err(RuntimeError::Type(format!(
                "Cannot access field '{}' on non-record value {}",
                field_name,
                self.value_to_display_string(&value)
            ))),
        }
    }

    fn eval_list_access(
        &mut self,
        value: Value,
        index_expr: &Expression,
    ) -> Result<Value, RuntimeError> {
        match value {
            Value::List(elements) => {
                let index_value = self.eval_expr(index_expr)?;
                match index_value {
                    Value::Integer(idx) => {
                        if idx < 0 {
                            return Err(RuntimeError::Type(format!("Index {} out of bounds", idx)));
                        }
                        let idx = idx as usize;
                        elements
                            .get(idx)
                            .cloned()
                            .ok_or(RuntimeError::Type(format!("Index {} out of bounds", idx)))
                    }
                    _ => Err(RuntimeError::Type(
                        "List index must be an integer".to_string(),
                    )),
                }
            }
            Value::String(s) => {
                let index_value = self.eval_expr(index_expr)?;
                match index_value {
                    Value::Integer(idx) => {
                        if idx < 0 || idx as usize >= s.len() {
                            return Err(RuntimeError::Type(format!("Index {} out of bounds", idx)));
                        }
                        let ch = s.chars().nth(idx as usize).unwrap();
                        Ok(Value::String(ch.to_string()))
                    }
                    _ => Err(RuntimeError::Type(
                        "String index must be an integer".to_string(),
                    )),
                }
            }
            _ => Err(RuntimeError::Type(
                "Cannot index non-list value".to_string(),
            )),
        }
    }

    fn eval_record_literal(
        &mut self,
        record_literal: &RecordLiteral,
    ) -> Result<Value, RuntimeError> {
        let mut fields = HashMap::new();
        for field_init in &record_literal.fields {
            let value = self.eval_expr(&field_init.value)?;
            fields.insert(field_init.name.clone(), value);
        }
        Ok(Value::Record(fields))
    }

    fn pattern_matches(&self, pattern: &Pattern, value: &Value) -> Result<bool, RuntimeError> {
        match pattern {
            Pattern::Wildcard(_) => Ok(true),
            Pattern::Identifier(name, _) => {
                // Check if it's a variant
                if let Value::Variant { name: v_name, data } = value {
                    if v_name == name && data.is_none() {
                        return Ok(true);
                    }
                }
                // Otherwise it's a binding variable
                Ok(true)
            }
            Pattern::Literal(lit, _) => {
                let pattern_value = match lit {
                    LiteralValue::Integer(i) => Value::Integer(*i),
                    LiteralValue::Float(f) => Value::Float(*f),
                    LiteralValue::String(s) => Value::String(s.clone()),
                    LiteralValue::Boolean(b) => Value::Boolean(*b),
                    LiteralValue::None => Value::Unit,
                };
                Ok(&pattern_value == value)
            }
            Pattern::Variant { name, patterns, .. } => {
                if let Value::Variant { name: v_name, data } = value {
                    if v_name != name {
                        return Ok(false);
                    }
                    if let Some(inner_patterns) = patterns {
                        if let Some(variant_data) = data {
                            if inner_patterns.len() == 1 {
                                return self.pattern_matches(&inner_patterns[0], variant_data);
                            }
                        }
                        return Ok(false);
                    }
                    Ok(data.is_none())
                } else {
                    Ok(false)
                }
            }
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern, value: Value) -> Result<(), RuntimeError> {
        match pattern {
            Pattern::Wildcard(_) => Ok(()),
            Pattern::Identifier(name, _) => {
                // Don't bind if it's a variant match
                if let Value::Variant { name: v_name, data } = &value {
                    if v_name == name && data.is_none() {
                        return Ok(());
                    }
                }
                self.env.define(name.clone(), value);
                Ok(())
            }
            Pattern::Literal(_, _) => Ok(()),
            Pattern::Variant { patterns, .. } => {
                if let Some(inner_patterns) = patterns {
                    if let Value::Variant {
                        data: Some(variant_data),
                        ..
                    } = value
                    {
                        if inner_patterns.len() == 1 {
                            self.bind_pattern(&inner_patterns[0], *variant_data)?;
                        }
                    }
                }
                Ok(())
            }
        }
    }

    fn apply_unary_op(&self, op: UnaryOperator, right: Value) -> Result<Value, RuntimeError> {
        match op {
            UnaryOperator::Minus => match right {
                Value::Integer(i) => Ok(Value::Integer(-i)),
                Value::Float(f) => Ok(Value::Float(-f)),
                _ => Err(RuntimeError::Type(
                    "Unary minus can only be applied to integers and floats".into(),
                )),
            },
            UnaryOperator::Not => Ok(Value::Boolean(!self.is_truthy(&right))),
            UnaryOperator::Plus => match right {
                Value::Integer(i) => Ok(Value::Integer(i)),
                Value::Float(f) => Ok(Value::Float(f)),
                _ => Err(RuntimeError::Type(
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
        &mut self,
        left: Value,
        op: BinaryOperator,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        if matches!(op, BinaryOperator::And | BinaryOperator::Or) {
            return match (&left, &right) {
                (Value::Boolean(l), Value::Boolean(r)) => match op {
                    BinaryOperator::And => Ok(Value::Boolean(*l && *r)),
                    BinaryOperator::Or => Ok(Value::Boolean(*l || *r)),
                    _ => unreachable!(),
                },
                _ => Err(RuntimeError::Type(
                    "Type mismatch in binary operation".into(),
                )),
            };
        }

        match (left, right) {
            (Value::Integer(l), Value::Integer(r)) => match op {
                BinaryOperator::Add => Ok(Value::Integer(l.wrapping_add(r))),
                BinaryOperator::Subtract => Ok(Value::Integer(l.wrapping_sub(r))),
                BinaryOperator::Multiply => Ok(Value::Integer(l.wrapping_mul(r))),
                BinaryOperator::Divide => {
                    if r == 0 {
                        return Err(RuntimeError::DivisionByZero);
                    }
                    Ok(Value::Integer(l / r))
                }
                BinaryOperator::Modulo => {
                    if r == 0 {
                        return Err(RuntimeError::DivisionByZero);
                    }
                    Ok(Value::Integer(l.rem_euclid(r)))
                }
                BinaryOperator::Equal => Ok(Value::Boolean(l == r)),
                BinaryOperator::NotEqual => Ok(Value::Boolean(l != r)),
                BinaryOperator::LessThan => Ok(Value::Boolean(l < r)),
                BinaryOperator::LessThanEqual => Ok(Value::Boolean(l <= r)),
                BinaryOperator::GreaterThan => Ok(Value::Boolean(l > r)),
                BinaryOperator::GreaterThanEqual => Ok(Value::Boolean(l >= r)),
                _ => Err(RuntimeError::Type(format!(
                    "Invalid integer operator: {:?}",
                    op
                ))),
            },
            (Value::Float(l), Value::Float(r)) => match op {
                BinaryOperator::Add => Ok(Value::Float(l + r)),
                BinaryOperator::Subtract => Ok(Value::Float(l - r)),
                BinaryOperator::Multiply => Ok(Value::Float(l * r)),
                BinaryOperator::Divide => Ok(Value::Float(l / r)),
                BinaryOperator::Modulo => Ok(Value::Float(l % r)),
                BinaryOperator::Equal => Ok(Value::Boolean((l - r).abs() < f64::EPSILON)),
                BinaryOperator::NotEqual => Ok(Value::Boolean((l - r).abs() >= f64::EPSILON)),
                BinaryOperator::LessThan => Ok(Value::Boolean(l < r)),
                BinaryOperator::LessThanEqual => Ok(Value::Boolean(l <= r)),
                BinaryOperator::GreaterThan => Ok(Value::Boolean(l > r)),
                BinaryOperator::GreaterThanEqual => Ok(Value::Boolean(l >= r)),
                _ => Err(RuntimeError::Type("Invalid float operator".into())),
            },
            (Value::Float(l), Value::Integer(r)) => match op {
                BinaryOperator::Divide => Ok(Value::Float(l / r as f64)),
                BinaryOperator::Modulo => Ok(Value::Float(l % r as f64)),
                _ => Err(RuntimeError::Type("Invalid float operator".into())),
            },
            (Value::String(l), Value::String(r)) => match op {
                BinaryOperator::Add => Ok(Value::String(format!("{}{}", l, r))),
                BinaryOperator::Equal => Ok(Value::Boolean(l == r)),
                BinaryOperator::NotEqual => Ok(Value::Boolean(l != r)),
                _ => Err(RuntimeError::Type("Invalid string operator".into())),
            },
            (Value::List(l), Value::List(r)) => match op {
                BinaryOperator::Add => {
                    let mut new_list = l.clone();
                    new_list.extend(r);
                    Ok(Value::List(new_list))
                }
                BinaryOperator::Equal => Ok(Value::Boolean(l == r)),
                BinaryOperator::NotEqual => Ok(Value::Boolean(l != r)),
                _ => Err(RuntimeError::Type("Invalid list operator".into())),
            },
            _ => Err(RuntimeError::Type(
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

    /*=================== HELPERS & BUILT-INS ============================ */

    pub fn value_to_display_string(&self, value: &Value) -> String {
        match value {
            Value::Integer(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.clone(),
            Value::Boolean(b) => b.to_string(),
            Value::Unit => "()".to_string(),
            Value::List(items) => {
                let items_str: Vec<String> = items
                    .iter()
                    .map(|v| self.value_to_display_string(v))
                    .collect();
                format!("[{}]", items_str.join(", "))
            }
            Value::Record(fields) => {
                let fields_str: Vec<String> = fields
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, self.value_to_display_string(v)))
                    .collect();
                format!("{{{}}}", fields_str.join(", "))
            }
            Value::Function { name, .. } => {
                format!(
                    "<function {}>",
                    name.as_ref().unwrap_or(&"anonymous".to_string())
                )
            }
            Value::File { path, .. } => format!("<file '{}'>", path),
            Value::Args { .. } => format!("CLI arguments: {:?}", self.args).to_string(),
            Value::Variant { name, data } => {
                if let Some(d) = data {
                    format!("{}({})", name, self.value_to_display_string(d))
                } else {
                    name.clone()
                }
            }
            Value::Range {
                start,
                end,
                inclusive,
            } => {
                if *inclusive {
                    format!("{}..={}", start, end)
                } else {
                    format!("{}..<{}", start, end)
                }
            }
            Value::Map(map) => {
                let entries: Vec<String> = map
                    .iter()
                    .map(|(k, v)| {
                        format!(
                            "{}: {}",
                            self.value_to_display_string(&k.to_value()),
                            self.value_to_display_string(v)
                        )
                    })
                    .collect();
                format!("{{{}}}", entries.join(", "))
            }
            _ => format!("{:?}", value),
        }
    }

    fn open_file(&mut self, path: String, mode_str: String) -> Result<Value, RuntimeError> {
        use std::fs::OpenOptions;

        let mode = match mode_str.as_str() {
            "r" => FileMode::Read,
            "w" => FileMode::Write,
            "a" => FileMode::Append,
            _ => {
                return Err(RuntimeError::Type(format!(
                    "Invalid file mode: {}",
                    mode_str
                )));
            }
        };

        let file = match mode {
            FileMode::Read => OpenOptions::new().read(true).open(&path),
            FileMode::Write => OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open(&path),
            FileMode::Append => OpenOptions::new()
                .write(true)
                .create(true)
                .append(true)
                .open(&path),
        };

        match file {
            Ok(f) => {
                let id = self.next_fd;
                self.files.push(Some(f));
                self.next_fd += 1;
                Ok(Value::File {
                    id,
                    path: path.clone(),
                    mode,
                    closed: false,
                })
            }
            Err(e) => Err(RuntimeError::Type(format!(
                "Failed to open file '{}': {}",
                path, e
            ))),
        }
    }

    fn file_read(&mut self, id: usize) -> Result<Value, RuntimeError> {
        use std::io::Read;

        if id >= self.files.len() || self.files[id].is_none() {
            return Err(RuntimeError::Type("Invalid file descriptor".into()));
        }

        let mut content = String::new();
        if let Some(file) = &mut self.files[id] {
            file.read_to_string(&mut content)
                .map_err(|e| RuntimeError::Type(format!("Failed to read file: {}", e)))?;
        }

        Ok(Value::String(content))
    }

    fn file_read_lines(&mut self, id: usize) -> Result<Value, RuntimeError> {
        use std::io::{BufRead, BufReader};

        if id >= self.files.len() || self.files[id].is_none() {
            return Err(RuntimeError::Type("Invalid file descriptor".into()));
        }

        let lines: Vec<Value> = if let Some(file) = &self.files[id] {
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

    fn file_write(&mut self, id: usize, text: &str) -> Result<Value, RuntimeError> {
        use std::io::Write;

        if id >= self.files.len() || self.files[id].is_none() {
            return Err(RuntimeError::Type("Invalid file descriptor".into()));
        }

        if let Some(file) = &mut self.files[id] {
            file.write_all(text.as_bytes())
                .map_err(|e| RuntimeError::Type(format!("Failed to write to file: {}", e)))?;
        }

        Ok(Value::Unit)
    }

    fn file_close(&mut self, id: usize) -> Result<Value, RuntimeError> {
        if id >= self.files.len() {
            return Err(RuntimeError::Type("Invalid file descriptor".into()));
        }

        self.files[id] = None;
        Ok(Value::Unit)
    }
}
