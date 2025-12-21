# TASK: Refactor Built-in Types to Lexer-Level Tokens

## Context

Your compiler currently treats built-in types (`int`, `bool`, `string`, `float`,
`unit`, `any`) as identifiers that are resolved to semantic types during type
checking. This creates unnecessary overhead and complexity. The lexer should
recognize these as first-class tokens.

### Current State
- Lexer outputs `TokenType::Identifier("int")`
- Parser creates `ast::Type::Named("int", span)`
- Type checker resolves string `"int"` → `types::Type::Int`

### Target State
- Lexer outputs `TokenType::IntType`
- Parser creates `ast::Type::Int(span)`
- Type checker maps directly to `types::Type::Int`

## Objective

Move built-in type recognition from the type checker to the lexer, eliminating string-based type resolution and making built-in types first-class syntactic elements.
Ensure the code compiles after our refactor.

## Refactoring Steps

### Step 1: Extend TokenType Enum
Add new token variants for each built-in type to your `TokenType` enum. Update
the `Display` implementation to handle these new variants.

### Step 2: Update Lexer Keyword Recognition
Modify the lexer's identifier scanning logic to recognize built-in type names
before falling back to generic identifiers. Add cases for "int", "bool",
"string", "float", "unit", and "any" that produce the corresponding type tokens.

### Step 3: Refactor AST Type Representation
Simplify your AST type system to include direct variants for built-in types.
Remove or reduce the `TypePrimary` enum since built-in types no longer need
string-based representation. Ensure all type nodes in theAST can carry span information
for error reporting.

### Step 4: Update Parser Type Parsing
Rewrite the parser's type parsing function to handle the new token types. Create
match arms for each built-in type token that constructs the corresponding AST
type node. Ensure user-defined types (identifiers) still parse correctly for
custom type names.

### Step 5: Simplify Type Checker Resolution
Refactor the type checker's type resolution logic to directly map AST built-in
type variants to semantic types. Remove all string-based type resolution for
built-ins. Keep only the logic for resolving user-defined type names and generic
types.

### Step 6: Update AST Node Definitions
Audit all AST structures that store type information (parameters, variable
bindings, function signatures, field declarations) and ensure they use the
refactored type representation consistently.

### Step 7: Update Test Suite
Modify parser tests to assert on the new direct type variants instead of
string-based type names. Update test expectations to match the new token types.

## Verification Steps

1. **Compilation**: `cargo check` passes with zero errors
3. **Lexer Validation**: Verify `int` lexes as `IntType`, not `Identifier`
4. **Parser Validation**: Confirm type annotations parse to direct type nodes
5. **Type Checker Validation**: Ensure no string matching for built-in types remains
6. **Code Search**: Confirm no string literals for built-in types exist in type checker

## Benefits

- **Performance**: Eliminates string comparisons during type resolution
- **Error Detection**: Typos in type names caught at lexing stage
- **Simplicity**: Removes ~50+ lines of string-based resolution code
- **Architecture**: Built-in types become true syntactic primitives
- **Tooling**: Enables better syntax highlighting and IDE support

## Pitfalls to Avoid

1. **Span Preservation**: Ensure all type nodes retain span information for accurate error reporting
2. **Generic Types**: Generic type names (e.g., `Map` in `Map[int, string]`) must remain as identifiers
3. **User-Defined Types**: Custom type names should continue to parse as identifiers and resolve through symbol table lookup
4. **Keyword Precedence**: Built-in type tokens must be matched before the generic identifier fallback

## Success Criteria

- Built-in types are lexed as distinct tokens
- Parser constructs direct type nodes without string indirection for built-in types
- Type checker contains zero string comparisons for built-in type resolution
