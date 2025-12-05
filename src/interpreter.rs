use crate::ast::*;
use crate::builtins::eval_method;
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
    pub(crate) program: String,                  // argv[0]
    pub(crate) values: Vec<String>,              // positional args
    pub(crate) flags: HashMap<String, bool>,     // --flag, -f
    pub(crate) options: HashMap<String, String>, // --key=value, --key value
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
    // Interpreter state visible to built-ins
    pub(crate) env: Environment,
    pub(crate) files: Vec<Option<std::fs::File>>, // fd table
    pub(crate) next_fd: usize,                    // next available fd
    pub(crate) args: Args,                        // parsed cmdline
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
            Expression::Block(block) => self.eval_block(block),
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
            // Scope handling is now inside eval_block
            self.eval_block(&if_expr.then_branch)
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
            // eval_block manages its own scope
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
                    // For loop binds variable in current scope (or we could make a new scope)
                    // Existing logic bound in current. To be safe/clean for loops,
                    // we usually want a scope per iteration, or at least a scope for the loop.
                    // But eval_block creates a scope.
                    // So we bind in the *outer* scope (surrounding the block).
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
                // Match arm creates a scope
                let previous = self.env.clone();
                self.env = self.env.enclose();

                self.bind_pattern(&arm.pattern, value.clone())?;

                let result = match &arm.body {
                    ExpressionOrBlock::Expression(expr) => self.eval_expr(expr),
                    ExpressionOrBlock::Block(block) => {
                        // eval_block creates ANOTHER scope. That's fine.
                        // But we need to use a helper that DOESN'T create a scope
                        // if we want the match bindings to be visible in the block without a double-layer.
                        // Actually, double layer is fine.
                        self.eval_block(block)
                    }
                };

                self.env = previous;
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
        let previous = self.env.clone();
        self.env = self.env.enclose();

        // Use a closure to easily handle environment restoration
        let result = (|| {
            for stmt in &block.statements {
                self.eval_statement(stmt)?;
            }

            if let Some(final_expr) = &block.final_expression {
                self.eval_expr(final_expr)
            } else {
                Ok(Value::Unit)
            }
        })();

        self.env = previous;
        result
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
        // Evaluate arguments from AST to Values
        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.eval_expr(arg)?);
        }

        if let Value::Function {
            name: Some(name), ..
        } = &func_value
        {
            match name.as_str() {
                "print" => {
                    if arg_values.len() != 1 {
                        return Err(RuntimeError::Type("print expects 1 argument".into()));
                    }
                    println!("{}", self.value_to_display_string(&arg_values[0]));
                    return Ok(Value::Unit);
                }
                "eprint" => {
                    if arg_values.len() != 1 {
                        return Err(RuntimeError::Type("eprint expects 1 argument".into()));
                    }
                    eprintln!("{}", self.value_to_display_string(&arg_values[0]));
                    return Ok(Value::Unit);
                }
                "open" => {
                    if arg_values.len() != 2 {
                        return Err(RuntimeError::Type("open expects 2 arguments".into()));
                    }
                    if let (Value::String(p), Value::String(m)) = (&arg_values[0], &arg_values[1]) {
                        return self.open_file(p.clone(), m.clone());
                    }
                    return Err(RuntimeError::Type("open arguments must be strings".into()));
                }
                "input" => {
                    use std::io::{self, Write};
                    if arg_values.len() > 1 {
                        return Err(RuntimeError::Type("input expects 0 or 1 argument".into()));
                    }
                    if arg_values.len() == 1 {
                        print!("{}", self.value_to_display_string(&arg_values[0]));
                        io::stdout().flush().ok();
                    }
                    let mut line = String::new();
                    io::stdin()
                        .read_line(&mut line)
                        .map_err(|_| RuntimeError::Type("Failed to read from stdin".into()))?;
                    return Ok(Value::String(line.trim_end_matches('\n').to_string()));
                }
                "Map" => {
                    if !arg_values.is_empty() {
                        return Err(RuntimeError::Type("Map() takes no arguments".into()));
                    }
                    return Ok(Value::Map(HashMap::new()));
                }
                _ => {} // Continue to normal call
            }
        }

        if let Value::BuiltInMethod { receiver, method } = func_value {
            // For methods, we need to pass back to builtins module
            // But wait, eval_builtin_method expects AST expressions in the old code?
            return eval_method(self, *receiver, &method, arg_values, var_name);
        }

        // It's a standard function call
        self.eval_function_call_value(func_value, &arg_values)
    }

    // Evaluate a function call
    // public for use in built-in methods (.map, .filter, etc.) which need to call user functions
    pub(crate) fn eval_function_call_value(
        &mut self,
        func_value: Value,
        arg_values: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Value::Function {
            name,
            params,
            body,
            env,
        } = func_value
        {
            if let Some(func_name) = &name {
                if func_name.ends_with("Constructor") {
                    let variant_name = func_name.trim_end_matches("Constructor");
                    // Constructors for variants with data always take 1 argument
                    if arg_values.len() == 1 {
                        return Ok(Value::Variant {
                            name: variant_name.to_string(),
                            data: Some(Box::new(arg_values[0].clone())),
                        });
                    }
                }
            }
            if params.len() != arg_values.len() {
                return Err(RuntimeError::Type(format!(
                    "Function expects {} arguments, got {}",
                    params.len(),
                    arg_values.len()
                )));
            }

            let mut call_env = env.enclose();

            if let Some(func_name) = &name {
                call_env.define(
                    func_name.clone(),
                    Value::Function {
                        name: name.clone(),
                        params: params.clone(),
                        body: body.clone(),
                        env: env.clone(),
                    },
                );
            }

            for (param, arg_value) in params.iter().zip(arg_values.iter()) {
                call_env.define(param.name.clone(), arg_value.clone());
            }

            let previous = self.env.clone();
            self.env = call_env;

            let result = match self.eval_block(&body) {
                Ok(val) => Ok(val),
                Err(RuntimeError::Return(val)) => Ok(val),
                Err(e) => Err(e),
            };

            self.env = previous;
            result
        } else {
            Err(RuntimeError::Type("Cannot call non-function value".into()))
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
            Value::File { .. }
                if matches!(
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
                    format!("{}..{}", start, end)
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
}
