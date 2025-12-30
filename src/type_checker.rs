use crate::ast::*;
use crate::builtins;
use crate::types::{SymbolInfo, Type};
use std::collections::HashMap;
use std::iter::zip;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq)]
pub enum TypeError {
    #[error("Type mismatch: expected {expected:?}, got {actual:?}")]
    TypeMismatch { expected: Type, actual: Type },

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Cannot assign to immutable variable '{0}'")]
    ImmutableAssignment(String),

    #[error("Unknown type: {0}")]
    UnknownType(String),

    #[error("Function '{name}' expects {expected} arguments, got {actual}")]
    ArityMismatch {
        name: String,
        expected: usize,
        actual: usize,
    },

    #[error("Call to non-function type: {0:?}")]
    NotAFunction(Type),

    #[error("Property '{field}' does not exist on type {ty:?}")]
    InvalidPropertyAccess { ty: Type, field: String },

    #[error("Return statement outside of function")]
    ReturnOutsideFunction,

    #[error("Non-boolean condition in control flow")]
    NonBooleanCondition,
}

pub struct TypeEnv {
    scopes: Vec<HashMap<String, SymbolInfo>>,
    functions: HashMap<String, Type>,
    type_definitions: HashMap<String, Type>,
    return_types: Vec<Type>,
}

impl TypeEnv {
    pub fn new() -> Self {
        let mut env = TypeEnv {
            scopes: vec![HashMap::new()],
            functions: HashMap::new(),
            type_definitions: HashMap::new(),
            return_types: Vec::new(),
        };
        env.inject_builtins();
        env
    }

    fn inject_builtins(&mut self) {
        let registry = builtins::BuiltinRegistry::new();

        for (name, ty) in registry.global_functions {
            self.functions.insert(name, ty);
        }

        for (name, info) in registry.global_variables {
            if let Some(scope) = self.scopes.last_mut() {
                scope.insert(name, info);
            }
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define_variable(&mut self, name: String, ty: Type, mutable: bool) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, SymbolInfo { ty, mutable });
        }
    }

    pub fn lookup_variable(&self, name: &str) -> Option<&SymbolInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info);
            }
        }
        None
    }

    pub fn define_function(&mut self, name: String, ty: Type) {
        self.functions.insert(name, ty);
    }

    pub fn lookup_function(&self, name: &str) -> Option<&Type> {
        self.functions.get(name)
    }

    pub fn lookup_callable(&self, name: &str) -> Option<Type> {
        if let Some(sym) = self.lookup_variable(name) {
            return Some(sym.ty.clone());
        }
        self.lookup_function(name).cloned()
    }

    pub fn define_type(&mut self, name: String, ty: Type) {
        self.type_definitions.insert(name, ty);
    }

    pub fn lookup_type(&self, name: &str) -> Option<&Type> {
        self.type_definitions.get(name)
    }
}

pub type Substitution = HashMap<String, Type>;

pub struct TypeChecker {
    env: TypeEnv,
    subst: Substitution,
    fresh_id_counter: usize,
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            env: TypeEnv::new(),
            subst: HashMap::new(),
            fresh_id_counter: 0,
        }
    }

    fn fresh_type_var(&mut self, prefix: &str) -> Type {
        self.fresh_id_counter += 1;
        Type::TypeVar(format!("{}_{}", prefix, self.fresh_id_counter))
    }

    fn instantiate(&mut self, t: Type) -> Type {
        if let Type::Poly(vars, inner) = t {
            let mut mapping = HashMap::new();
            for var in vars {
                mapping.insert(var.clone(), self.fresh_type_var(&var));
            }
            self.apply_mapping(*inner, &mapping)
        } else {
            t
        }
    }

    fn apply_mapping(&self, t: Type, mapping: &HashMap<String, Type>) -> Type {
        match t {
            Type::TypeVar(ref n) => {
                if let Some(replacement) = mapping.get(n) {
                    replacement.clone()
                } else {
                    t
                }
            }
            Type::List(inner) => Type::List(Box::new(self.apply_mapping(*inner, mapping))),
            Type::Map(k, v) => Type::Map(
                Box::new(self.apply_mapping(*k, mapping)),
                Box::new(self.apply_mapping(*v, mapping)),
            ),
            Type::Function(params, ret) => Type::Function(
                params.into_iter().map(|p| self.apply_mapping(p, mapping)).collect(),
                Box::new(self.apply_mapping(*ret, mapping)),
            ),
            Type::Record(fields) => {
                let mut new_fields = HashMap::new();
                for (k, v) in fields {
                    new_fields.insert(k, self.apply_mapping(v, mapping));
                }
                Type::Record(new_fields)
            }
            // Poly nested? Skip for now
            _ => t,
        }
    }

    fn apply_subst(&self, t: Type) -> Type {
        match t {
            Type::TypeVar(ref n) => {
                if let Some(replacement) = self.subst.get(n) {
                    self.apply_subst(replacement.clone())
                } else {
                    t
                }
            }
            Type::List(inner) => Type::List(Box::new(self.apply_subst(*inner))),
            Type::Map(k, v) => Type::Map(Box::new(self.apply_subst(*k)), Box::new(self.apply_subst(*v))),
            Type::Function(params, ret) => Type::Function(
                params.into_iter().map(|p| self.apply_subst(p)).collect(),
                Box::new(self.apply_subst(*ret)),
            ),
            Type::Record(fields) => {
                let mut new_fields = HashMap::new();
                for (k, v) in fields {
                    new_fields.insert(k, self.apply_subst(v));
                }
                Type::Record(new_fields)
            }
            Type::Poly(vars, inner) => {
                // TODO: Handle bound variables properly to avoid capture
                Type::Poly(vars, Box::new(self.apply_subst(*inner)))
            }
            _ => t,
        }
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), TypeError> {
        self.harvest_definitions(&program.statements)?;
        for stmt in &program.statements {
            self.check_top_statement(stmt)?;
        }
        Ok(())
    }

    fn harvest_definitions(&mut self, stmts: &[TopStatement]) -> Result<(), TypeError> {
        for stmt in stmts {
            match stmt {
                TopStatement::TypeDecl(decl) => {
                    self.harvest_type_decl(decl)?;
                }
                TopStatement::LetStmt(LetStatement::Function(func)) => {
                    let mut param_types = Vec::new();
                    for param in &func.params {
                        param_types.push(self.resolve_ast_type(&param.ty)?);
                    }
                    let return_type = self.resolve_ast_type(&func.return_type)?;
                    self.env.define_function(
                        func.name.clone(),
                        Type::Function(param_types, Box::new(return_type)),
                    );
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn harvest_type_decl(&mut self, decl: &TypeDeclaration) -> Result<(), TypeError> {
        match &decl.constructor {
            TypeConstructor::Record(record_type) => {
                let mut fields = HashMap::new();
                for field in &record_type.fields {
                    fields.insert(field.name.clone(), self.resolve_ast_type(&field.ty)?);
                }
                self.env
                    .define_type(decl.name.clone(), Type::Record(fields));
            }
            TypeConstructor::Alias(ty) => {
                let resolved = self.resolve_ast_type(ty)?;
                self.env.define_type(decl.name.clone(), resolved);
            }
            TypeConstructor::Sum(sum) => {
                self.env
                    .define_type(decl.name.clone(), Type::Variant(decl.name.clone()));

                for variant in &sum.variants {
                    if let Some(inner_ty_ast) = &variant.ty {
                        let inner_ty = self.resolve_ast_type(inner_ty_ast)?;
                        self.env.define_function(
                            variant.name.clone(),
                            Type::Function(
                                vec![inner_ty],
                                Box::new(Type::Variant(decl.name.clone())),
                            ),
                        );
                    } else {
                        self.env.define_variable(
                            variant.name.clone(),
                            Type::Variant(decl.name.clone()),
                            false,
                        );
                    }
                }
            }
        }
        Ok(())
    }

    fn resolve_ast_type(&self, ast_type: &crate::ast::Type) -> Result<Type, TypeError> {
        match ast_type {
            crate::ast::Type::Int(_) => Ok(Type::Int),
            crate::ast::Type::Float(_) => Ok(Type::Float),
            crate::ast::Type::String(_) => Ok(Type::String),
            crate::ast::Type::Bool(_) => Ok(Type::Bool),
            crate::ast::Type::Unit(_) => Ok(Type::Unit),
            crate::ast::Type::Any(_) => Ok(Type::Any),
            crate::ast::Type::Inferred(_) => Ok(Type::Unknown), // Handle parser's placeholder
            crate::ast::Type::Named(name, _) => {
                if let Some(ty) = self.env.lookup_type(name) {
                    Ok(ty.clone())
                } else {
                    Err(TypeError::UnknownType(name.clone()))
                }
            }
            crate::ast::Type::List(inner, _) => {
                let inner_ty = self.resolve_ast_type(inner)?;
                Ok(Type::List(Box::new(inner_ty)))
            }
            crate::ast::Type::Record(rec) => {
                let mut fields = HashMap::new();
                for f in &rec.fields {
                    fields.insert(f.name.clone(), self.resolve_ast_type(&f.ty)?);
                }
                Ok(Type::Record(fields))
            }
            crate::ast::Type::Generic { name, args, .. } => match name.as_str() {
                "Map" | "map" => {
                    if args.len() == 2 {
                        let k = self.resolve_ast_type(&args[0])?;
                        let v = self.resolve_ast_type(&args[1])?;
                        Ok(Type::Map(Box::new(k), Box::new(v)))
                    } else {
                        Err(TypeError::UnknownType("Map requires 2 arguments".into()))
                    }
                }
                _ => Err(TypeError::UnknownType(format!("Unknown generic {}", name))),
            },
            crate::ast::Type::Function {
                params,
                return_type,
                ..
            } => {
                let mut p_types = Vec::new();
                for p in params {
                    p_types.push(self.resolve_ast_type(p)?);
                }
                let r_type = self.resolve_ast_type(return_type)?;
                Ok(Type::Function(p_types, Box::new(r_type)))
            }
        }
    }

    fn check_top_statement(&mut self, stmt: &TopStatement) -> Result<(), TypeError> {
        match stmt {
            TopStatement::TypeDecl(_) => Ok(()),
            TopStatement::LetStmt(stmt) => self.check_let_statement(stmt, None),
            TopStatement::Expression(expr) => {
                self.check_expr(&expr.expression)?;
                Ok(())
            }
        }
    }

    fn check_let_statement(
        &mut self,
        stmt: &LetStatement,
        return_ctx: Option<&Type>,
    ) -> Result<(), TypeError> {
        match stmt {
            LetStatement::Variable(binding) => {
                // Check if variable exists in CURRENT scope only (not parent scopes)
                if let Some(scope) = self.env.scopes.last() {
                    if let Some(existing) = scope.get(&binding.name) {
                        // Variable exists in current scope
                        if !existing.mutable {
                            return Err(TypeError::ImmutableAssignment(binding.name.clone()));
                        }
                        // If it's mutable, we're allowing reassignment with let statement
                        // This is similar to shadowing behavior
                    }
                }

                let val_type = self.check_expr_with_context(&binding.value, return_ctx)?;
                let final_type = if let Some(annotation) = &binding.type_annotation {
                    let declared = self.resolve_ast_type(annotation)?;
                    self.unify(&declared, &val_type)
                        .ok_or_else(|| TypeError::TypeMismatch {
                            expected: declared,
                            actual: val_type,
                        })?
                } else {
                    val_type
                };
                self.env
                    .define_variable(binding.name.clone(), final_type, binding.mutable);
                Ok(())
            }
            LetStatement::Function(func) => {
                // First, compute and register the function's type signature
                let mut param_types = Vec::new();
                for param in &func.params {
                    param_types.push(self.resolve_ast_type(&param.ty)?);
                }
                let return_type = self.resolve_ast_type(&func.return_type)?;

                // Register the function in the current scope (as a variable with function type)
                // This allows:
                // 1. The function to be called later in the same scope
                // 2. Recursive calls within the function itself
                self.env.define_variable(
                    func.name.clone(),
                    Type::Function(param_types.clone(), Box::new(return_type.clone())),
                    func.mutable,
                );

                // Type check function body in its own scope
                self.env.enter_scope();
                for (param, p_type) in zip(&func.params, &param_types) {
                    // Make parameters mutable by default to allow list modification
                    self.env.define_variable(param.name.clone(), p_type.clone(), true);
                }

                let old_return_types = std::mem::take(&mut self.env.return_types);

                // Check the body - populates return_types vector in typing context for all possible
                // returns in the function (including implicit return from last stmt)
                let block_type = self.check_block(&func.body, Some(&return_type))?;

                // Unify the return statements if more than one
                let actual_ret = if !self.env.return_types.is_empty() {
                    let mut unified_ret = return_type.clone();
                    let return_types = self.env.return_types.clone();
                    for ret_ty in &return_types {
                        unified_ret = self.unify(&unified_ret, ret_ty).ok_or_else(|| {
                            TypeError::TypeMismatch {
                                expected: unified_ret.clone(),
                                actual: ret_ty.clone(),
                            }
                        })?;
                    }
                    unified_ret
                } else {
                    block_type // Implicit return by last stmt
                };

                // Verify the actual return type matches the declared type
                self.expect_type(&return_type, &actual_ret)?;

                // If declared return type was Unknown (inferred), update the function signature
                if return_type == Type::Unknown {
                    self.env.define_function(
                        func.name.clone(),
                        Type::Function(param_types, Box::new(actual_ret)),
                    );
                }

                self.env.return_types = old_return_types;

                self.env.exit_scope();
                Ok(())
            }
        }
    }

    fn check_stmt(&mut self, stmt: &Statement, return_ctx: Option<&Type>) -> Result<(), TypeError> {
        match stmt {
            Statement::Let(let_stmt) => self.check_let_statement(let_stmt, return_ctx),
            Statement::Expression(expr_stmt) => {
                self.check_expr_with_context(&expr_stmt.expression, return_ctx)?;
                Ok(())
            }
            Statement::Return(expr_opt, _) => {
                let actual = if let Some(expr) = expr_opt {
                    self.check_expr_with_context(expr, return_ctx)?
                } else {
                    Type::Unit
                };

                self.env.return_types.push(actual.clone());

                if let Some(expected) = return_ctx {
                    self.expect_type(expected, &actual)
                } else {
                    Err(TypeError::ReturnOutsideFunction)
                }
            }
            Statement::Break(_) | Statement::Continue(_) => Ok(()),
        }
    }

    fn check_block(&mut self, block: &Block, return_ctx: Option<&Type>) -> Result<Type, TypeError> {
        self.env.enter_scope();
        let mut diverges = false;
        for stmt in &block.statements {
            self.check_stmt(stmt, return_ctx)?;
            match stmt {
                Statement::Return(..) | Statement::Break(_) | Statement::Continue(_) => {
                    diverges = true;
                }
                _ => {}
            }
        }
        let result = if diverges {
            Type::Any
        } else if let Some(final_expr) = &block.final_expression {
            self.check_expr_with_context(final_expr, return_ctx)?
        } else {
            Type::Unit
        };
        self.env.exit_scope();
        Ok(result)
    }

    fn check_expr(&mut self, expr: &Expression) -> Result<Type, TypeError> {
        self.check_expr_with_context(expr, None)
    }

    fn check_expr_with_context(
        &mut self,
        expr: &Expression,
        return_ctx: Option<&Type>,
    ) -> Result<Type, TypeError> {
        match expr {
            Expression::Primary(p) => self.check_primary(p, return_ctx),
            Expression::Binary(b) => self.check_binary(b, return_ctx),
            Expression::Unary(u) => self.check_unary(u, return_ctx),
            Expression::If(if_expr) => {
                let cond_ty = self.check_expr_with_context(&if_expr.condition, return_ctx)?;
                self.expect_type(&Type::Bool, &cond_ty)?;

                let then_ty = self.check_block(&if_expr.then_branch, return_ctx)?;

                if let Some(else_branch) = &if_expr.else_branch {
                    let else_ty = self.check_expr_with_context(else_branch, return_ctx)?;
                    self.unify(&then_ty, &else_ty)
                        .ok_or(TypeError::TypeMismatch {
                            expected: then_ty,
                            actual: else_ty,
                        })
                } else {
                    Ok(Type::Unit)
                }
            }
            Expression::Block(block) => self.check_block(block, return_ctx),
            Expression::While(w) => {
                let cond = self.check_expr_with_context(&w.condition, return_ctx)?;
                self.expect_type(&Type::Bool, &cond)?;
                self.check_block(&w.body, return_ctx)?;
                Ok(Type::Unit)
            }
            Expression::For(f) => {
                let iterable = self.check_expr_with_context(&f.iterable, return_ctx)?;
                let item_type = match iterable {
                    Type::List(inner) => *inner,
                    Type::Range { .. } => Type::Int,
                    Type::Map(k, _) => *k,
                    Type::Any | Type::Unknown => Type::Any,
                    _ => {
                        return Err(TypeError::TypeMismatch {
                            expected: Type::List(Box::new(Type::Any)),
                            actual: iterable,
                        });
                    }
                };

                self.env.enter_scope();
                self.bind_pattern_type(&f.pattern, item_type)?;
                self.check_block(&f.body, return_ctx)?;
                self.env.exit_scope();
                Ok(Type::Unit)
            }
            Expression::Postfix(p) => self.check_postfix(p, return_ctx),
            Expression::Range(r) => {
                let start = self.check_expr_with_context(&r.start, return_ctx)?;
                let end = self.check_expr_with_context(&r.end, return_ctx)?;
                self.expect_type(&Type::Int, &start)?;
                self.expect_type(&Type::Int, &end)?;
                Ok(Type::Range(Box::new(Type::Int)))
            }
            Expression::Lambda(l) => {
                self.env.enter_scope();
                let mut param_types = Vec::new();
                for p in &l.params {
                    let ty = self.resolve_ast_type(&p.ty)?;
                    self.env.define_variable(p.name.clone(), ty.clone(), false);
                    param_types.push(ty);
                }

                let old_return_types = std::mem::take(&mut self.env.return_types);

                let body_ty = match &l.body {
                    ExpressionOrBlock::Block(b) => self.check_block(b, None)?,
                    ExpressionOrBlock::Expression(e) => self.check_expr_with_context(e, None)?,
                };

                if let Some(ret_ann) = &l.return_type_annotation {
                    let expected = self.resolve_ast_type(ret_ann)?;
                    self.expect_type(&expected, &body_ty)?;
                }

                self.env.return_types = old_return_types;

                self.env.exit_scope();
                Ok(Type::Function(param_types, Box::new(body_ty)))
            }
            Expression::Match(m) => {
                let val_type = self.check_expr_with_context(&m.value, return_ctx)?;
                let mut result_type: Option<Type> = None;

                for arm in &m.arms {
                    self.env.enter_scope();
                    self.bind_pattern_type(&arm.pattern, val_type.clone())?;

                    let arm_ty = match &arm.body {
                        ExpressionOrBlock::Block(b) => self.check_block(b, return_ctx)?,
                        ExpressionOrBlock::Expression(e) => {
                            self.check_expr_with_context(e, return_ctx)?
                        }
                    };
                    self.env.exit_scope();

                    if let Some(prev) = &result_type {
                        result_type = Some(self.unify(prev, &arm_ty).ok_or_else(|| {
                            TypeError::TypeMismatch {
                                expected: prev.clone(),
                                actual: arm_ty.clone(),
                            }
                        })?);
                    } else {
                        result_type = Some(arm_ty);
                    }
                }
                Ok(result_type.unwrap_or(Type::Unit))
            }
        }
    }

    fn check_binary(
        &mut self,
        b: &BinaryExpression,
        return_ctx: Option<&Type>,
    ) -> Result<Type, TypeError> {
        if matches!(
            b.operator,
            BinaryOperator::Assign
                | BinaryOperator::AddAssign
                | BinaryOperator::SubtractAssign
                | BinaryOperator::MultiplyAssign
                | BinaryOperator::DivideAssign
                | BinaryOperator::ModuloAssign
        ) {
            let lhs = &b.left;
            match &**lhs {
                Expression::Primary(PrimaryExpression::Identifier(name, _)) => {
                    let rhs_ty = self.check_expr_with_context(&b.right, return_ctx)?;

                    // Check if variable exists
                    if let Some(info) = self.env.lookup_variable(name) {
                        // Variable exists - check mutability
                        let target_ty = info.ty.clone();
                        let is_mutable = info.mutable;

                        if !is_mutable && b.operator == BinaryOperator::Assign {
                            return Err(TypeError::ImmutableAssignment(name.clone()));
                        }

                        self.expect_type(&target_ty, &rhs_ty)?;
                    } else {
                        // Variable doesn't exist - implicit declaration (immutable)
                        if b.operator != BinaryOperator::Assign {
                            return Err(TypeError::UndefinedVariable(name.clone()));
                        }
                        self.env.define_variable(name.clone(), rhs_ty, false);
                    }

                    return Ok(Type::Unit);
                }
                Expression::Postfix(p) => {
                    let root_name = self.extract_root_identifier(&p.primary)?;
                    let info = self
                        .env
                        .lookup_variable(&root_name)
                        .ok_or_else(|| TypeError::UndefinedVariable(root_name.clone()))?;

                    if !info.mutable {
                        return Err(TypeError::ImmutableAssignment(root_name));
                    }

                    let lhs_ty = self.check_expr_with_context(lhs, return_ctx)?;
                    let rhs_ty = self.check_expr_with_context(&b.right, return_ctx)?;
                    self.expect_type(&lhs_ty, &rhs_ty)?;
                    return Ok(Type::Unit);
                }
                _ => return Err(TypeError::UnknownType("Invalid assignment target".into())),
            }
        }

        let left = self.check_expr_with_context(&b.left, return_ctx)?;
        let right = self.check_expr_with_context(&b.right, return_ctx)?;

        match b.operator {
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide
            | BinaryOperator::Modulo => {
                // Use unify to handle Unknown types
                let is_int = self.unify(&left, &Type::Int).is_some()
                    && self.unify(&right, &Type::Int).is_some();
                let is_float = self.unify(&left, &Type::Float).is_some()
                    && self.unify(&right, &Type::Float).is_some();

                if is_int {
                    Ok(Type::Int)
                } else if is_float {
                    Ok(Type::Float)
                } else if b.operator == BinaryOperator::Add
                    && self.unify(&left, &Type::String).is_some()
                    && self.unify(&right, &Type::String).is_some()
                {
                    Ok(Type::String)
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: left,
                        actual: right,
                    })
                }
            }
            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                if self.unify(&left, &right).is_some() {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: left,
                        actual: right,
                    })
                }
            }
            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => {
                let is_int = self.unify(&left, &Type::Int).is_some()
                    && self.unify(&right, &Type::Int).is_some();
                let is_float = self.unify(&left, &Type::Float).is_some()
                    && self.unify(&right, &Type::Float).is_some();

                if is_int || is_float {
                    Ok(Type::Bool)
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        actual: right,
                    })
                }
            }
            BinaryOperator::And | BinaryOperator::Or => {
                self.expect_type(&Type::Bool, &left)?;
                self.expect_type(&Type::Bool, &right)?;
                Ok(Type::Bool)
            }
            _ => Ok(Type::Unit),
        }
    }

    fn check_unary(
        &mut self,
        u: &UnaryExpression,
        return_ctx: Option<&Type>,
    ) -> Result<Type, TypeError> {
        let ty = self.check_expr_with_context(&u.right, return_ctx)?;
        match u.operator {
            UnaryOperator::Not => {
                self.expect_type(&Type::Bool, &ty)?;
                Ok(Type::Bool)
            }
            UnaryOperator::Minus | UnaryOperator::Plus => {
                if ty == Type::Int || ty == Type::Float {
                    Ok(ty)
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: Type::Int,
                        actual: ty,
                    })
                }
            }
        }
    }

    fn check_primary(
        &mut self,
        p: &PrimaryExpression,
        return_ctx: Option<&Type>,
    ) -> Result<Type, TypeError> {
        match p {
            PrimaryExpression::Literal(lit, _) => match lit {
                LiteralValue::Integer(_) => Ok(Type::Int),
                LiteralValue::Float(_) => Ok(Type::Float),
                LiteralValue::String(_) => Ok(Type::String),
                LiteralValue::Boolean(_) => Ok(Type::Bool),
                LiteralValue::None => Ok(Type::Unit),
            },
            PrimaryExpression::Identifier(name, _) => self
                .env
                .lookup_callable(name)
                .ok_or_else(|| TypeError::UndefinedVariable(name.clone())),
            PrimaryExpression::Parenthesized(e, _) => self.check_expr_with_context(e, return_ctx),
            PrimaryExpression::List(l) => {
                if l.elements.is_empty() {
                    return Ok(Type::List(Box::new(Type::Unknown)));
                }
                let first_ty = self.check_expr_with_context(&l.elements[0], return_ctx)?;
                for e in &l.elements[1..] {
                    let ty = self.check_expr_with_context(e, return_ctx)?;
                    if self.unify(&first_ty, &ty).is_none() {
                        return Err(TypeError::TypeMismatch {
                            expected: Type::List(Box::new(first_ty)),
                            actual: Type::List(Box::new(ty)),
                        });
                    }
                }
                Ok(Type::List(Box::new(first_ty)))
            }
            PrimaryExpression::Record(r) => {
                let mut fields = HashMap::new();
                for f in &r.fields {
                    let ty = self.check_expr_with_context(&f.value, return_ctx)?;
                    fields.insert(f.name.clone(), ty);
                }
                Ok(Type::Record(fields))
            }
            PrimaryExpression::This(_) => Ok(Type::Any),
        }
    }

    fn extract_root_identifier(&self, expr: &Expression) -> Result<String, TypeError> {
        match expr {
            Expression::Primary(PrimaryExpression::Identifier(name, _)) => Ok(name.clone()),
            Expression::Postfix(p) => self.extract_root_identifier(&p.primary),
            _ => Err(TypeError::UnknownType(
                "Cannot determine root variable".into(),
            )),
        }
    }

    fn check_postfix(
        &mut self,
        p: &PostfixExpression,
        return_ctx: Option<&Type>,
    ) -> Result<Type, TypeError> {
        let mut current_ty = self.check_expr_with_context(&p.primary, return_ctx)?;

        // Track the root variable name for refinement
        let root_var_name =
            if let Expression::Primary(PrimaryExpression::Identifier(name, _)) = &*p.primary {
                Some(name.clone())
            } else {
                None
            };

        for (idx, op) in p.operators.iter().enumerate() {
            match op {
                PostfixOperator::Call { args, .. } => {
                    // Instantiate generic functions
                    if let Type::Poly(..) = current_ty {
                        current_ty = self.instantiate(current_ty);
                    }

                    match current_ty.clone() {
                        Type::Function(param_types, ret_type) => {
                            if args.len() != param_types.len() {
                                return Err(TypeError::ArityMismatch {
                                    name: "anonymous".into(),
                                    expected: param_types.len(),
                                    actual: args.len(),
                                });
                            }

                            // Check arguments and collect their actual types
                            let mut actual_arg_types = Vec::new();
                            for (arg_expr, expected_ty) in args.iter().zip(param_types.iter()) {
                                let arg_ty = self.check_expr_with_context(arg_expr, return_ctx)?;

                                // Try to unify - this might refine Unknown types
                                if let Some(unified) = self.unify(expected_ty, &arg_ty) {
                                    actual_arg_types.push(unified);
                                    // Check that the unification is valid
                                    self.expect_type(expected_ty, &arg_ty)?;
                                } else {
                                    return Err(TypeError::TypeMismatch {
                                        expected: expected_ty.clone(),
                                        actual: arg_ty,
                                    });
                                }
                            }

                            // Refine return type for generic methods
                            let refined_ret_type = *ret_type.clone();

                            // Type refinement for mutating methods on variables
                            if idx == 1 {
                                if let Some(PostfixOperator::FieldAccess {
                                    name: method_name,
                                    ..
                                }) = p.operators.get(0)
                                {
                                    if let Some(var_name) = &root_var_name {
                                        if matches!(
                                            method_name.as_str(),
                                            "insert" | "push" | "append"
                                        ) {
                                            // For mutation methods, we might want to refine the COLLECTION's type
                                            // based on what's being inserted, but the method call ITSELF returns Unit.
                                            let info_opt = self.env.lookup_variable(var_name).cloned();
                                            if let Some(info) = info_opt {
                                                if info.mutable {
                                                    // Get the argument types to potentially refine the collection type
                                                    // E.g. if we push an Int into List<Unknown>, it becomes List<Int>
                                                    let refined_collection_ty = match (
                                                        &info.ty,
                                                        method_name.as_str(),
                                                    ) {
                                                        (Type::List(inner), "push" | "append")
                                                            if actual_arg_types.len() == 1 =>
                                                        {
                                                            if let Some(new_inner) = self
                                                                .unify(inner, &actual_arg_types[0])
                                                            {
                                                                Some(Type::List(Box::new(
                                                                    new_inner,
                                                                )))
                                                            } else {
                                                                None
                                                            }
                                                        }
                                                        (Type::Map(k, v), "insert")
                                                            if actual_arg_types.len() == 2 =>
                                                        {
                                                            if let (Some(new_k), Some(new_v)) = (
                                                                self.unify(k, &actual_arg_types[0]),
                                                                self.unify(v, &actual_arg_types[1]),
                                                            ) {
                                                                Some(Type::Map(
                                                                    Box::new(new_k),
                                                                    Box::new(new_v),
                                                                ))
                                                            } else {
                                                                None
                                                            }
                                                        }
                                                        _ => None,
                                                    };

                                                    if let Some(new_ty) = refined_collection_ty {
                                                        self.env.define_variable(
                                                            var_name.clone(),
                                                            new_ty,
                                                            true,
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            current_ty = refined_ret_type;
                        }
                        Type::Any | Type::Unknown => {
                            current_ty = Type::Unknown;
                        }
                        _ => return Err(TypeError::NotAFunction(current_ty)),
                    }
                }
                PostfixOperator::FieldAccess { name, .. } => {
                    match &current_ty {
                        Type::Record(fields) => {
                            current_ty = fields.get(name).cloned().ok_or_else(|| {
                                TypeError::InvalidPropertyAccess {
                                    ty: current_ty.clone(),
                                    field: name.clone(),
                                }
                            })?;
                        }
                        Type::Any | Type::Unknown => {
                            current_ty = Type::Unknown;
                        }
                        // Use builtin method types
                        _ => {
                            current_ty = builtins::get_builtin_method_type(&current_ty, name)
                                .ok_or_else(|| TypeError::InvalidPropertyAccess {
                                    ty: current_ty.clone(),
                                    field: name.clone(),
                                })?;
                        }
                    }
                }
                PostfixOperator::ListAccess { index, .. } => {
                    let idx_ty = self.check_expr_with_context(index, return_ctx)?;
                    self.expect_type(&Type::Int, &idx_ty)?;
                    match current_ty {
                        Type::List(inner) => current_ty = *inner,
                        Type::String => current_ty = Type::String,
                        Type::Any | Type::Unknown => current_ty = Type::Unknown,
                        _ => {
                            return Err(TypeError::TypeMismatch {
                                expected: Type::List(Box::new(Type::Any)),
                                actual: current_ty,
                            });
                        }
                    }
                }
                PostfixOperator::TypePath { .. } => {
                    // Handle :: operator - keep current type for now
                }
            }
        }
        Ok(current_ty)
    }

    fn bind_pattern_type(&mut self, pattern: &Pattern, val_type: Type) -> Result<(), TypeError> {
        match pattern {
            Pattern::Identifier(name, _) => {
                self.env.define_variable(name.clone(), val_type, false);
                Ok(())
            }
            Pattern::Wildcard(_) => Ok(()),
            Pattern::Variant { name, patterns, .. } => {
                if let Some(func_ty) = self.env.lookup_function(name) {
                    if let Type::Function(param_types, _ret) = func_ty {
                        // Clone param_types to release the immutable borrow before calling bind_pattern_type
                        let param_types = param_types.clone();
                        if let Some(pats) = patterns {
                            if pats.len() != param_types.len() {
                                return Err(TypeError::TypeMismatch {
                                    expected: Type::Variant("?".into()),
                                    actual: val_type,
                                });
                            }
                            for (pat, ty) in pats.iter().zip(param_types.iter()) {
                                self.bind_pattern_type(pat, ty.clone())?;
                            }
                        }
                        Ok(())
                    } else {
                        Ok(())
                    }
                } else {
                    Ok(())
                }
            }

            Pattern::Literal(lit, _) => {
                let lit_ty = match lit {
                    LiteralValue::Integer(_) => Type::Int,
                    LiteralValue::String(_) => Type::String,
                    LiteralValue::Boolean(_) => Type::Bool,
                    _ => Type::Any,
                };
                self.expect_type(&lit_ty, &val_type)
            }
        }
    }

    fn expect_type(&mut self, expected: &Type, actual: &Type) -> Result<(), TypeError> {
        if self.unify(expected, actual).is_some() {
            Ok(())
        } else {
            let expected = self.apply_subst(expected.clone());
            let actual = self.apply_subst(actual.clone());
            Err(TypeError::TypeMismatch {
                expected,
                actual,
            })
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> Option<Type> {
        let t1 = self.apply_subst(t1.clone());
        let t2 = self.apply_subst(t2.clone());

        if t1 == t2 {
            return Some(t1);
        }
        match (t1.clone(), t2.clone()) {
            (Type::TypeVar(n), t) | (t, Type::TypeVar(n)) => {
                if let Type::TypeVar(n2) = &t {
                    if n == *n2 {
                        return Some(t);
                    }
                }
                // Simple occurs check could go here
                self.subst.insert(n, t.clone());
                Some(t)
            }
            (Type::Any, _) => Some(t2),
            (_, Type::Any) => Some(t1),
            (Type::Unknown, _) => Some(t2),
            (_, Type::Unknown) => Some(t1),
            (Type::List(i1), Type::List(i2)) => {
                let inner = self.unify(&i1, &i2)?;
                Some(Type::List(Box::new(inner)))
            }
            (Type::Map(k1, v1), Type::Map(k2, v2)) => {
                let key = self.unify(&k1, &k2)?;
                let val = self.unify(&v1, &v2)?;
                Some(Type::Map(Box::new(key), Box::new(val)))
            }
            (Type::Record(f1), Type::Record(f2)) => {
                if f1.len() != f2.len() {
                    return None;
                }
                let mut unified_fields = HashMap::new();
                for (name, ty1) in f1 {
                    if let Some(ty2) = f2.get(&name) {
                        unified_fields.insert(name.clone(), self.unify(&ty1, ty2)?);
                    } else {
                        return None;
                    }
                }
                Some(Type::Record(unified_fields))
            }
            (Type::Function(p1, r1), Type::Function(p2, r2)) => {
                if p1.len() != p2.len() {
                    return None;
                }
                let mut unified_params = Vec::new();
                for (pt1, pt2) in p1.iter().zip(p2.iter()) {
                    unified_params.push(self.unify(pt1, pt2)?);
                }
                let unified_ret = self.unify(&r1, &r2)?;
                Some(Type::Function(unified_params, Box::new(unified_ret)))
            }
            _ => None,
        }
    }
}
