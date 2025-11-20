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
    Bool, // Added for completeness
    Unit, // Added for void functions
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
    // New: Supports arr[i] = value
    ArrayAssignment {
        array: Expression,
        index: Expression,
        value: Expression,
    },
    VarDecl {
        name: String,
        type_annotation: TypeAnnotation,
        value: Option<Expression>,
    },
    FunctionDef(FunctionDef),
    StructDecl(StructDecl),
    EnumDecl(EnumDecl),
    // New: Control Flow
    While {
        condition: Box<Expression>,
        body: Vec<Statement>,
    },
    For {
        iterator: String,
        iterable: Box<Expression>,
        body: Vec<Statement>,
    },
    // Updated: Return is now optional (void returns)
    Return(Option<Expression>),
    Break,
    Continue,
}

// --- Match Specific Structures ---

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard, // The '_' pattern
    Literal(LiteralValue),
    Identifier(String),
    EnumVariant {
        enum_name: String,
        variant: String,
        vars: Vec<Pattern>, // Recursive patterns for Option::Some(x)
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub types: Vec<TypeAnnotation>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<(String, TypeAnnotation)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub args: Vec<String>,
    pub body: Vec<Statement>,
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
    // New: Unary Operations (!true, -5)
    Unary {
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
    Block(Vec<Statement>),
    Match {
        value: Box<Expression>,
        arms: Vec<MatchArm>,
    },
}

/// Represents a literal value in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Unit,
    Array(Vec<LiteralValue>),
}

/// Represents an operator in a binary or unary expression.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    // Binary Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo, // Added

    // Comparison
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    // Logic
    And, // Added
    Or,  // Added

    // Unary
    Not, // Added
         // Negate uses 'Subtract' usually, or you can add specific 'Negate'
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Integer(i) => write!(f, "{}", i),
            LiteralValue::Float(fl) => write!(f, "{}", fl),
            LiteralValue::String(s) => write!(f, "\"{}\"", s),
            LiteralValue::Boolean(b) => write!(f, "{}", b),
            LiteralValue::Unit => write!(f, "()"),
            LiteralValue::Array(elements) => {
                let elems: Vec<String> = elements.iter().map(|e| format!("{}", e)).collect();
                write!(f, "[{}]", elems.join(", "))
            }
        }
    }
}

impl fmt::Display for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn {}({}) {{ ... }}", self.name, self.args.join(", "))
    }
}
