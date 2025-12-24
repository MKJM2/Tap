use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Unit,

    List(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Record(HashMap<String, Type>),

    Function(Vec<Type>, Box<Type>),

    Variant(String),

    Range(Box<Type>),

    // For type inference
    TypeVar(String),
    Poly(Vec<String>, Box<Type>), // Polymorphic type (Scheme): <T, U> Type

    // TODO: Do we need these? For type inference algo..?
    Unknown,
    Any,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymbolInfo {
    pub ty: Type,
    pub mutable: bool,
}
