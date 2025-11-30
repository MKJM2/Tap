use std::fmt;

/// Represents a span of code in the source file, from `start` to `end` character offset.
#[derive(Debug, Clone, PartialEq, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Span { start, end }
    }
}

/// Represents a program, which is a collection of top-level statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<TopStatement>,
    pub span: Span,
}

impl Program {
    pub fn new(statements: Vec<TopStatement>, span: Span) -> Self {
        Program { statements, span }
    }
}

/// Represents a top-level statement in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum TopStatement {
    TypeDecl(TypeDeclaration),
    LetStmt(LetStatement),
    Expression(ExpressionStatement),
}

impl TopStatement {
    pub fn span(&self) -> Span {
        match self {
            TopStatement::TypeDecl(decl) => decl.span,
            TopStatement::LetStmt(stmt) => stmt.span(),
            TopStatement::Expression(expr_stmt) => expr_stmt.span,
        }
    }
}

/// Represents a type declaration: `type <type_name> = <type_ctor>`
#[derive(Debug, Clone, PartialEq)]
pub struct TypeDeclaration {
    pub name: String,
    pub constructor: TypeConstructor,
    pub span: Span,
}

/// Represents the right-hand side of a type declaration.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeConstructor {
    Sum(SumConstructor),
    Record(RecordType),
}

impl TypeConstructor {
    pub fn span(&self) -> Span {
        match self {
            TypeConstructor::Sum(sum) => sum.span,
            TypeConstructor::Record(record) => record.span,
        }
    }
}

/// Represents a sum type constructor: `<variant> ( "|" <variant> )*`
#[derive(Debug, Clone, PartialEq)]
pub struct SumConstructor {
    pub variants: Vec<Variant>,
    pub span: Span,
}

/// Represents a variant in a sum type: `<identifier> "(" <type> ")" | <identifier>`
#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: String,
    pub ty: Option<Type>, // Optional type for variants with data
    pub span: Span,
}

/// Represents a let statement, which can be a function binding or a variable binding.
#[derive(Debug, Clone, PartialEq)]
pub enum LetStatement {
    Function(FunctionBinding),
    Variable(VariableBinding),
}

impl LetStatement {
    pub fn span(&self) -> Span {
        match self {
            LetStatement::Function(func) => func.span,
            LetStatement::Variable(var) => var.span,
        }
    }
}

/// Represents a function binding: `<opt_mut> <identifier> <param_list> ":" <type> "=" <block>`
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionBinding {
    pub mutable: bool,
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
    pub span: Span,
}

/// Represents a variable binding: `<opt_mut> <identifier> ( ":" <type> )? "=" <expr> ";"`
#[derive(Debug, Clone, PartialEq)]
pub struct VariableBinding {
    pub mutable: bool,
    pub name: String,
    pub type_annotation: Option<Type>,
    pub value: Expression,
    pub span: Span,
}

/// Represents an expression statement: `<expr> ";"`
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
    pub span: Span,
}

/// Represents a block of statements and an optional final expression.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub final_expression: Option<Box<Expression>>,
    pub span: Span,
}

/// Represents a statement within a block.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    Expression(ExpressionStatement),
}

impl Statement {
    pub fn span(&self) -> Span {
        match self {
            Statement::Let(let_stmt) => let_stmt.span(),
            Statement::Expression(expr_stmt) => expr_stmt.span,
        }
    }
}

/// Represents a parameter in a function or lambda definition.
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// Represents a type in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Function {
        params: Vec<Type>, // Types of parameters
        return_type: Box<Type>,
        span: Span,
    },
    Primary(TypePrimary),
    // TODO: Add other complex types as needed
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Type::Function { span, .. } => *span,
            Type::Primary(primary) => primary.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypePrimary {
    Named(String, Span), // e.g., "Int", "String", "MyStruct"
    Generic {
        name: String,
        arg: Box<Type>,
        span: Span,
    }, // e.g., "Option[Int]"
    Record(RecordType),
    List(Box<Type>, Span), // e.g., "[Int]"
}

impl TypePrimary {
    pub fn span(&self) -> Span {
        match self {
            TypePrimary::Named(_, span) => *span,
            TypePrimary::Generic { span, .. } => *span,
            TypePrimary::Record(record) => record.span,
            TypePrimary::List(_, span) => *span,
        }
    }
}

/// Represents a record (struct) type definition.
#[derive(Debug, Clone, PartialEq)]
pub struct RecordType {
    pub fields: Vec<FieldDeclaration>,
    pub span: Span,
}

/// Represents a field declaration within a record type.
#[derive(Debug, Clone, PartialEq)]
pub struct FieldDeclaration {
    pub name: String,
    pub ty: Type,
    pub span: Span,
}

/// Represents an expression in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    If(IfExpression),
    While(WhileExpression),
    For(ForExpression),
    Match(MatchExpression),
    Lambda(LambdaExpression),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
    Postfix(PostfixExpression),
    Primary(PrimaryExpression),
    Block(Block), // A block can be an expression if it returns a value.
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::If(expr) => expr.span,
            Expression::While(expr) => expr.span,
            Expression::For(expr) => expr.span,
            Expression::Match(expr) => expr.span,
            Expression::Lambda(expr) => expr.span,
            Expression::Binary(expr) => expr.span,
            Expression::Unary(expr) => expr.span,
            Expression::Postfix(expr) => expr.span,
            Expression::Primary(expr) => expr.span(),
            Expression::Block(block) => block.span,
        }
    }
}

/// Represents an if expression: `"if" "(" <expr> ")" <block> ( "else" <block> )?`
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub then_branch: Block,
    pub else_branch: Option<Block>,
    pub span: Span,
}

/// Represents a while expression: `"while" "(" <expr> ")" <block>`
#[derive(Debug, Clone, PartialEq)]
pub struct WhileExpression {
    pub condition: Box<Expression>,
    pub body: Block,
    pub span: Span,
}

/// Represents a for expression: `"for" <pattern> "in" <expr> <block>`
#[derive(Debug, Clone, PartialEq)]
pub struct ForExpression {
    pub pattern: Pattern,
    pub iterable: Box<Expression>,
    pub body: Block,
    pub span: Span,
}

/// Represents a match expression: `"match" "(" <expr> ")" "{" <match_arms> "}"`
#[derive(Debug, Clone, PartialEq)]
pub struct MatchExpression {
    pub value: Box<Expression>,
    pub arms: Vec<MatchArm>,
    pub span: Span,
}

/// Represents a match arm: `"|" <pattern> "=>" <expr_or_block> <opt_comma>`
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: ExpressionOrBlock,
    pub span: Span,
}

/// Represents either an expression or a block as a body of a match arm.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionOrBlock {
    Expression(Box<Expression>),
    Block(Block),
}

impl ExpressionOrBlock {
    pub fn span(&self) -> Span {
        match self {
            ExpressionOrBlock::Expression(expr) => expr.span(),
            ExpressionOrBlock::Block(block) => block.span,
        }
    }
}

/// Represents a pattern in a match arm.
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Wildcard(Span),           // "_"
    Identifier(String, Span), // e.g., "x"
    Variant {
        name: String,
        patterns: Option<Vec<Pattern>>, // For variants with data, e.g., Some(x)
        span: Span,
    },
}

impl Pattern {
    pub fn span(&self) -> Span {
        match self {
            Pattern::Wildcard(span) => *span,
            Pattern::Identifier(_, span) => *span,
            Pattern::Variant { span, .. } => *span,
        }
    }
}

/// Represents a lambda expression: `<param_list> ( ":" <type> )? "=>" <expr_or_block>`
#[derive(Debug, Clone, PartialEq)]
pub struct LambdaExpression {
    pub params: Vec<Parameter>,
    pub return_type_annotation: Option<Type>,
    pub body: ExpressionOrBlock,
    pub span: Span,
}

/// Represents a binary expression: `<unary_expr> ( <bin_op> <unary_expr> )*`
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
    pub span: Span,
}

/// Represents a unary expression: `( "+" | "-" | "!" )? <postfix_expr>`
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub operator: UnaryOperator,
    pub right: Box<Expression>,
    pub span: Span,
}

/// Represents a postfix expression: `<primary> (<postfix_op>)*`
#[derive(Debug, Clone, PartialEq)]
pub struct PostfixExpression {
    pub primary: Box<Expression>, // Changed from PrimaryExpression to Expression
    pub operators: Vec<PostfixOperator>,
    pub span: Span,
}

/// Represents a postfix operator.
#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOperator {
    Call { args: Vec<Expression>, span: Span }, // "(" <arg_list>? ")"
    FieldAccess { name: String, span: Span },   // "." <identifier>
    TypePath { name: String, span: Span },      // "::" <identifier>
    ListAccess { index: Box<Expression>, span: Span }, // "[" <expr> "]"
}

impl PostfixOperator {
    pub fn span(&self) -> Span {
        match self {
            PostfixOperator::Call { span, .. } => *span,
            PostfixOperator::FieldAccess { span, .. } => *span,
            PostfixOperator::TypePath { span, .. } => *span,
            PostfixOperator::ListAccess { span, .. } => *span,
        }
    }
}

/// Represents a primary expression.
#[derive(Debug, Clone, PartialEq)]
pub enum PrimaryExpression {
    Literal(LiteralValue, Span),
    Identifier(String, Span),
    This(Span),
    Parenthesized(Box<Expression>, Span), // "(" <expr> ")"
    List(ListLiteral),
    Record(RecordLiteral),
}

impl PrimaryExpression {
    pub fn span(&self) -> Span {
        match self {
            PrimaryExpression::Literal(_, span) => *span,
            PrimaryExpression::Identifier(_, span) => *span,
            PrimaryExpression::This(span) => *span,
            PrimaryExpression::Parenthesized(_, span) => *span,
            PrimaryExpression::List(list) => list.span,
            PrimaryExpression::Record(record) => record.span,
        }
    }
}

/// Represents a list literal: "[" (<expr> ("," <expr>)*)? "]"
#[derive(Debug, Clone, PartialEq)]
pub struct ListLiteral {
    pub elements: Vec<Expression>,
    pub span: Span,
}

/// Represents a record literal: "{" <field_init> ("," <field_init>)* "}"
#[derive(Debug, Clone, PartialEq)]
pub struct RecordLiteral {
    pub fields: Vec<FieldInitializer>,
    pub span: Span,
}

/// Represents a field initializer in a record literal: `<identifier> ":" <expr>`
#[derive(Debug, Clone, PartialEq)]
pub struct FieldInitializer {
    pub name: String,
    pub value: Expression,
    pub span: Span,
}

/// Represents a literal value in the language.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    None, // "None" keyword
}

/// Represents a binary operator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,

    // Comparison
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,

    // Logical
    And,
    Or,

    // Assignment with operation
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
}

/// Represents a unary operator.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not,
}

// Display implementations for easier debugging and printing

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Integer(i) => write!(f, "{}", i),
            LiteralValue::Float(fl) => write!(f, "{}", fl),
            LiteralValue::String(s) => write!(f, "\"{}\"", s),
            LiteralValue::Boolean(b) => write!(f, "{}", b),
            LiteralValue::None => write!(f, "None"),
        }
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "+"),
            BinaryOperator::Subtract => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Equal => write!(f, "=="),
            BinaryOperator::NotEqual => write!(f, "!="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterThanEqual => write!(f, ">="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessThanEqual => write!(f, "<="),
            BinaryOperator::And => write!(f, "&&"),
            BinaryOperator::Or => write!(f, "||"),
            BinaryOperator::AddAssign => write!(f, "+="),
            BinaryOperator::SubtractAssign => write!(f, "-="),
            BinaryOperator::MultiplyAssign => write!(f, "*="),
            BinaryOperator::DivideAssign => write!(f, "/="),
        }
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Plus => write!(f, "+"),
            UnaryOperator::Minus => write!(f, "-"),
            UnaryOperator::Not => write!(f, "!"),
        }
    }
}
