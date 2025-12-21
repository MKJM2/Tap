# Diagnosis of Cargo Test Failures

I have analyzed the failing tests in `tests/interpreter.rs` and the relevant source code in `src/type_checker.rs`, `src/builtins.rs`, `src/parser.rs`, and `src/lexer.rs`.

## Summary

The failing tests are:
1.  `interpreter_tests::test_aoc_2025_day1_part2` - `TypeMismatch { expected: List(Int), actual: Int }`
2.  `interpreter_tests::test_aoc_2025_day4_part2` - `TypeMismatch { expected: Int, actual: Int }`
3.  `interpreter_tests::test_snippet_find_unique_elements_first_element` - `TypeMismatch { expected: List(Int), actual: Bool }`

## Root Cause Analysis

### 1. `push` and `append` Return Type Discrepancy

There is a critical mismatch between the runtime behavior (`src/builtins.rs`) and the type checking logic (`src/type_checker.rs`) for list methods `push` and `append`.

*   **Runtime (`src/builtins.rs`):**
    ```rust
    "push" | "append" => {
        check_arg_count(1)?;
        // MUTATE IN PLACE
        list_rc.borrow_mut().push(args.swap_remove(0));
        Ok(Value::Unit) // Returns Unit
    }
    ```
    The definition of `get_list_method_type` also correctly specifies `Unit`:
    ```rust
    "push" | "append" => Some(Type::Function(
        vec![inner.clone()],
        Box::new(Type::Unit),
    )),
    ```

*   **Type Checker (`src/type_checker.rs`):**
    In `check_postfix`, there is logic that **overrides** the return type of `push` and `append` to be `List<T>` instead of `Unit`.
    ```rust
    "push" | "append" if actual_arg_types.len() == 1 => {
        // For List.push(T), return type should be List<T>
        Type::List(Box::new(actual_arg_types[0].clone()))
    }
    ```
    This causes the type checker to believe `list.push(item)` returns a list, while at runtime it returns `Unit`.

### 2. Analysis of Failing Tests

#### `test_aoc_2025_day1_part2`
**Error:** `TypeMismatch { expected: List(Int), actual: Int }`

The discrepancy in `push` likely confuses the type inference or context expectations. While the provided code snippet uses `turns.push(turn);` as a statement (which should be fine), the mismatch between expected `List(Int)` (likely from `get_turns` return type) and `actual: Int` is puzzling. It suggests that somewhere `Int` is being returned where `List(Int)` is expected. Given that `push` is typed as returning `List(Int)` by the checker, usages of it in expression positions would propagate this type.

#### `test_aoc_2025_day4_part2`
**Error:** `TypeMismatch { expected: Int, actual: Int }`

This error is highly unusual because `Type::Int` should match `Type::Int`. This implies one of the following:
1.  **Ambiguous `Debug` Output:** One of the types is NOT `Type::Int` but prints as `Int`. For example, if `Type::Variant` or another enum variant somehow printed as `Int`. However, `src/types.rs` uses derived `Debug`, so `Variant("Int")` would print as `Variant("Int")`.
2.  **Internal State Difference:** If `Type::Int` had associated data (like a Span) that differed, `PartialEq` would fail. But `Type::Int` is a unit variant.
3.  **Logical Contradiction:** If `expected` and `actual` are both `Type::Int`, `unify` returns `Some`, and `expect_type` succeeds. The failure implies `unify` returned `None`.

The most plausible explanation is that the error message is misleading due to `Debug` formatting or that one of the types is a `Variant` that coincidentally prints as `Int` (though unlikely with derived Debug). Alternatively, it could be `Type::Named("int")` resolving incorrectly, but the lexer correctly produces `KeywordInt` -> `Type::Int`.

#### `test_snippet_find_unique_elements_first_element`
**Error:** `TypeMismatch { expected: List(Int), actual: Bool }`

The snippet:
```tap
unique_elements(lst: [int]): [int] = {
    // ...
    if (!contains(uniques, element)) { ... }
    // ...
    return uniques;
};
unique_elements(...)[0];
```
The error `expected: List(Int), actual: Bool` suggests that `unique_elements` is inferred or checked to return `Bool` instead of `List(Int)`.
This could happen if `contains` (which returns `Bool`) is somehow interfering with the return type inference, or if the `push` override (returning `List(Int)`) interacts with the control flow in a way that the type checker misinterprets.

## Conclusion

The primary identified defect is the **incorrect return type override for `push`/`append` in `src/type_checker.rs`**. This causes a fundamental disagreement between the type checker and the runtime/built-in definitions. Fixing this is the first step. The "Int vs Int" error warrants further investigation after fixing the `push` return type, as it might be a symptom of a deeper issue with type representation or equality checks.
