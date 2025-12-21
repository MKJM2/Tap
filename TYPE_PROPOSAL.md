# Type System Proposal: Generics and Bidirectional Inference

The failing `test_lambda_body_mismatch` test highlights a limitation in the current `Tap` type checker: the inability to correctly infer and enforce types for generic higher-order functions like `map`.

Currently, built-in methods like `map` are defined with loose signatures using `Type::Any`:

```rust
// Current map signature
(T -> Any) -> List(Any)
```

This causes `l.map(...)` to return `List(Any)`, which effectively disables type checking for the result, allowing `List(Bool)` to be assigned to `List(String)` without error.

## Proposal

To support robust type checking for collection methods and lambda expressions, we need to introduce two key features:

### 1. Generic Type Variables

We need a way to represent "a type T that will be determined later".

**Changes:**
- Extend `Type` enum with a `TypeVar(String)` variant.
- Update `BuiltinRegistry` to use these type variables.

```rust
// Proposed map signature
// map: <T, U> (List<T>, (T) -> U) -> List<U>
```

### 2. Unification with Type Substitution

The `unify` function needs to be stateful or return a substitution map. When it encounters a `TypeVar`, it should "bind" that variable to the concrete type it matches against.

**Example Flow:**
1. `l` is `List(Int)`.
2. `l.map` is called. `T` binds to `Int`.
3. The lambda `(x: Int) => x > 1` is checked.
4. The lambda type `(Int) -> Bool` is unified with the expected argument type `(T) -> U`.
5. Since `T` is `Int`, the param matches.
6. The return type `Bool` binds to `TypeVar("U")`.
7. The result of `map` is instantiated as `List(U)`, which becomes `List(Bool)`.

### 3. Bidirectional Type Inference (Context Propagation)

To handle cases like `s: [string] = ...`, the type checker should push the *expected type* (`List(String)`) down into the expression being checked.

**Changes:**
- Ensure `check_expr_with_context` propagates the expected type into method calls.
- When checking `map`, if an expected type `List(String)` is known, we can infer that `U` must be `String`.
- We can then verify that the lambda returns `String`.

## Implementation Steps

1.  **Add `Type::TypeVariable(usize)`**: A unique ID for each type var.
2.  **Add `Type::GenericFunction`**: To represent functions that introduce new type variables (like `map` introduced `<U>`).
3.  **Implement `Substitution`**: A map from `TypeVariable` ID to `Type`.
4.  **Update `unify`**: To return a `Substitution` on success.
5.  **Refactor `TypeChecker`**: Maintain a set of active constraints and solve them (Hindley-Milner style or similar).

This will allow `test_lambda_body_mismatch` to fail correctly because `List(Bool)` will strictly NOT unify with `List(String)`.
