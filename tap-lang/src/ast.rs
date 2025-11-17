// src/ast.rs
use std::fmt;

/// Represents a program, which is a collection of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

/// Represents a type annotation in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Int,
    Str,
    Function {
        from: Box<TypeAnnotation>,
        to: Box<TypeAnnotation>,
    },
    Array(Box<TypeAnnotation>),
    Struct {
        name: String,
        fields: Vec<(String, TypeAnnotation)>,
    },
    Enum {
        name: String,
    },
    UserDefined(String),
    // TODO: Add other types
}

/// Represents a single statement in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expression(Expression),
    Assignment {
        name: String,
        value: Expression,
    },
    PropertyAssignment {
        object: Expression,
        property: String,
        value: Expression,
    },
    VarDecl {
        name: String,
        type_annotation: TypeAnnotation,
    },
    FunctionDef(FunctionDef),
    StructDecl(StructDecl),
    EnumDecl(EnumDecl),
    If {
        condition: Box<Expression>,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    Return(Expression),
    Break,
    Continue,
}

/// Represents a struct definition.
#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<(String, TypeAnnotation)>,
}

/// Represents an enum definition.
#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<String>,
}

/// Represents a function definition.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Statement>,
    // pub return_type: Option<TypeAnnotation>,
}

/// Represents a single expression in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(LiteralValue),
    Identifier(String),
    List(Vec<Expression>),
    Lambda {
        args: Vec<String>,
        body: Box<Expression>,
    },
    FunctionCall {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    Binary {
        left: Box<Expression>,
        op: Operator,
        right: Box<Expression>,
    },
    StructInstantiation {
        name: String,
        fields: Vec<(String, Expression)>,
    },
    Get {
        object: Box<Expression>,
        name: String,
    },
    Path {
        parts: Vec<String>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Vec<Statement>,
        else_branch: Option<Vec<Statement>>,
    },
    EnumVariant {
        enum_name: String,
        variant_name: String,
    },
    ArrayAccess {
        array: Box<Expression>,
        index: Box<Expression>,
    },
    // TODO:
    // Unary,
    // Grouping,
}

/// Represents a literal value in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(i64),
    String(String),
    Boolean(bool),
    // TODO:
    // Float(f64),
    // Bool(bool),
}

/// Represents an operator in a binary or unary expression.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    // Binary
    Add,
    Subtract,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    // TODO:
    // Modulo,
    // ...

    // Unary
    // Not,
    // Negate,
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Integer(i) => write!(f, "{} : int", i),
            LiteralValue::String(s) => write!(f, "\"{}\" : string", s),
            LiteralValue::Boolean(b) => write!(f, "{} : bool", b),
        }
    }
}

impl fmt::Display for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}({}) {{ ... }}", self.name, self.args.join(", "))
    }
}
