// tests/type_checker_tests.rs
use tap::diagnostics::Reporter;
use tap::lexer::Lexer;
use tap::parser::Parser;
use tap::type_checker::{TypeChecker, TypeError};

// --- Test Macros ---

/// Asserts that the provided source code passes type checking
macro_rules! assert_types_ok {
    ($name:ident, $source:expr) => {
        #[test]
        fn $name() {
            let source = $source;
            let mut reporter = Reporter::new();
            let tokens = Lexer::new(source, &mut reporter).tokenize().unwrap();
            let mut parser = Parser::new(&tokens, &mut reporter);
            let program = parser.parse_program().unwrap();

            let mut checker = TypeChecker::new();
            if let Err(e) = checker.check_program(&program) {
                eprintln!("{:?}", program);
                panic!("Type check failed unexpectedly: {:?}", e);
            }
        }
    };
}

/// Asserts that the provided source code FAILS type checking
/// Optionally matches against a specific error pattern
macro_rules! assert_types_err {
    ($name:ident, $source:expr) => {
        #[test]
        fn $name() {
            let source = $source;
            let mut reporter = Reporter::new();
            let tokens = Lexer::new(source, &mut reporter).tokenize().unwrap();
            let mut parser = Parser::new(&tokens, &mut reporter);
            let program = parser.parse_program().unwrap();

            let mut checker = TypeChecker::new();
            assert!(
                checker.check_program(&program).is_err(),
                "Expected type check failure, but succeeded"
            );
        }
    };
    ($name:ident, $source:expr, $error_pat:pat) => {
        #[test]
        fn $name() {
            let source = $source;
            let mut reporter = Reporter::new();
            let tokens = Lexer::new(source, &mut reporter).tokenize().unwrap();
            let mut parser = Parser::new(&tokens, &mut reporter);
            let program = parser.parse_program().unwrap();

            let mut checker = TypeChecker::new();
            match checker.check_program(&program) {
                Err($error_pat) => (), // Success
                Err(e) => panic!("Expected error matching pattern, got: {:?}", e),
                Ok(_) => panic!("Expected type check failure, but succeeded"),
            }
        }
    };
}

// --- BASIC TYPES ---

assert_types_ok!(
    test_basic_literals,
    "
    1;
    1.5;
    true;
    \"string\";
"
);

assert_types_err!(
    test_binop_mismatch,
    "
    1 + \"string\";
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_boolean_logic_mismatch,
    "
    true && 1;
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_variable_declaration_inference,
    "
    x = 10;
    y = x + 5;
"
);

assert_types_ok!(
    test_variable_declaration_explicit,
    "
    x: int = 10;
    y: string = \"hello\";
"
);

assert_types_err!(
    test_variable_declaration_mismatch,
    "
    x: int = \"hello\";
",
    TypeError::TypeMismatch { .. }
);

// --- MUTABILITY ---

assert_types_ok!(
    test_mutability_ok,
    "
    mut x = 10;
    x = 20;
"
);

assert_types_err!(
    test_mutability_violation,
    "
    x = 10;
    x = 20;
",
    TypeError::ImmutableAssignment(_)
);



// --- CONTROL FLOW ---

assert_types_ok!(
    test_if_condition,
    "
    if (true) { 1; };
    if (1 < 2) { 1; };
"
);

assert_types_err!(
    test_if_condition_non_bool,
    "
    if (1) { 1; };
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_while_loop,
    "
    while (true) { 1; };
"
);

assert_types_err!(
    test_while_condition_non_bool,
    "
    while (\"s\") { 1; };
",
    TypeError::TypeMismatch { .. }
);

// --- FUNCTIONS ---

assert_types_ok!(
    test_function_call_ok,
    "
    add(a: int, b: int): int = { a + b };
    res = add(1, 2);
"
);

assert_types_err!(
    test_function_arg_count_mismatch,
    "
    add(a: int, b: int): int = { a + b };
    add(1);
",
    TypeError::ArityMismatch { .. }
);

assert_types_err!(
    test_function_arg_type_mismatch,
    "
    add(a: int, b: int): int = { a + b };
    add(1, \"s\");
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_function_return_type_mismatch,
    "
    get_str(): string = { 123 };
",
    TypeError::TypeMismatch { .. }
);

// --- LISTS & MAPS (INFERENCE) ---

assert_types_ok!(
    test_list_inference,
    "
    l = [1, 2, 3];
    l.push(4);
"
);

assert_types_err!(
    test_list_mixed_types,
    "
    l = [1, \"s\"];
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_list_push_wrong_type,
    "
    l = [1, 2];
    l.push(\"s\");
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_map_basic,
    "
    mut m = Map();
    m.insert(\"key\", 1);
    val: int = m.get(\"key\");
"
);

assert_types_err!(
    test_map_value_mismatch,
    "
    mut m = Map();
    m.insert(\"key\", 1); // Infers Map<String, Int>
    m.insert(\"key2\", \"string\"); // Should fail
",
    TypeError::TypeMismatch { .. }
);

// --- SCOPING ---

assert_types_err!(
    test_shadowing,
    "
    mut x = 1;
    {
        x = \"string\"; // Shadowing with different type
    }
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_scope_leak,
    "
    if 1 > 0
    {
        inner = 1;
    }
    y = inner;
",
    TypeError::UndefinedVariable(_)
);

// --- RECURSION & FORWARD DECLARATION ---

assert_types_ok!(
    test_recursion_factorial,
    "
    fact(n: int): int = {
        if n <= 1 {
            1
        } else {
            n * fact(n - 1)
        }
    };
"
);

assert_types_ok!(
    test_mutual_recursion,
    "
    is_even(n: int): bool = {
        if n == 0 { true } else { is_odd(n - 1) }
    };

    is_odd(n: int): bool = {
        if (n == 0) { false } else { is_even(n - 1) }
    };
"
);

assert_types_err!(
    test_recursion_bad_arg,
    "
    fact(n: int): int = {
        if n <= 1 { 1 } else { fact(\"string\") }
    };
",
    TypeError::TypeMismatch { .. }
);

// --- NESTED COLLECTIONS ---

assert_types_ok!(
    test_nested_lists,
    "
    matrix = [[1, 2], [3, 4]];
    row: [int] = matrix[0];
    val: int = matrix[0][0];
"
);

assert_types_err!(
    test_nested_lists_mismatch,
    "
    // Inner lists must be consistent
    matrix = [[1, 2], [\"string\", \"string\"]];
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_map_of_lists,
    "
    mut m = Map();
    m.insert(\"evens\", [2, 4, 6]);
    m.insert(\"odds\", [1, 3, 5]);

    lst: [int] = m.get(\"evens\");
"
);

assert_types_ok!(
    test_list_of_records,
    "
    users = [
        { id: 1, name: \"Alice\" },
        { id: 2, name: \"Bob\" }
    ];
    u = users[0];
    n: string = u.name;
"
);

assert_types_err!(
    test_list_of_records_inconsistent,
    "
    users = [
        { id: 1, name: \"Alice\" },
        { id: 2, active: true } // Mismatched shape
    ];
",
    TypeError::TypeMismatch { .. }
);

// --- RECORDS & FIELDS ---

assert_types_ok!(
    test_record_access,
    "
    p = { x: 10, y: 20 };
    sum = p.x + p.y;
"
);

assert_types_err!(
    test_record_missing_field,
    "
    p = { x: 10, y: 20 };
    z = p.z; // Undefined field
",
    TypeError::InvalidPropertyAccess { .. }
);

assert_types_err!(
    test_record_field_type_mismatch,
    "
    p = { x: 10, name: \"center\" };
    math = p.name + 5; // Adding string to int
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_nested_record_access,
    "
    config = {
        server: { host: \"localhost\", port: 8080 },
        debug: true
    };
    p: int = config.server.port;
"
);

// --- INDEXING & MUTATION ---

assert_types_ok!(
    test_list_index_access,
    "
    l = [10, 20, 30];
    x = l[1]; // x is int
    y = x + 5;
"
);

assert_types_err!(
    test_list_index_with_string,
    "
    l = [1, 2];
    x = l[\"one\"];
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_list_mutation,
    "
    mut l = [1, 2];
    l[0] = 5;
"
);

assert_types_err!(
    test_list_mutation_wrong_type,
    "
    mut l = [1, 2];
    l[0] = \"string\"; // Can't put string in int list
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_immutable_list_mutation,
    "
    l = [1, 2];
    l[0] = 5; // l is not mut
",
    TypeError::ImmutableAssignment(_)
);

// --- CONTROL FLOW EXPRESSIONS ---

assert_types_ok!(
    test_if_expression_unified,
    "
    x: int = if (true) { 1 } else { 2 };
"
);

assert_types_err!(
    test_if_expression_mismatch,
    "
    x = if (true) { 1 } else { \"string\" };
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_block_returns,
    "
    // Block returns value of last expression
    x: int = {
        a = 5;
        a + 5
    };
"
);

assert_types_ok!(
    test_early_return_check,
    "
    check(x: int): int = {
        if (x < 0) {
            return 0; // Explicit return
        };
        x + 1 // Implicit block return
    };
"
);

assert_types_err!(
    test_bad_early_return,
    "
    check(x: int): int = {
        if (x < 0) {
            return \"error\"; // Wrong return type
        };
        x
    };
",
    TypeError::TypeMismatch { .. }
);

// --- HIGHER ORDER FUNCTIONS ---

assert_types_ok!(
    test_lambda_inference,
    "
    // map expects (T) -> U
    l = [1, 2, 3];
    s: [string] = l.map((x) => { x.to_string() });
"
);

assert_types_err!(
    test_lambda_body_mismatch,
    "
    l = [1, 2, 3];
    // Declared list of strings, but lambda returns bool
    // Explicitly annotate x as int to ensure mismatch
    s: [string] = l.map((x: int) => { x > 1 });
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_function_variable,
    "
    add(a: int, b: int): int = { a + b };
    op = add; // op is inferred as Function([int, int], int)
    res = op(5, 5);
"
);

assert_types_err!(
    test_call_non_function,
    "
    x = 1;
    x(5); // Error
",
    TypeError::NotAFunction(_)
);

// --- VARIANTS & PATTERN MATCHING ---

assert_types_ok!(
    test_variant_def_and_usage,
    "
    type Status = Active | Inactive | Suspended(string);

    s1 = Active;
    s2 = Suspended(\"violation\");
"
);

assert_types_ok!(
    test_match_expression_inference,
    "
    type OptionInt = Some(int) | None;

    val = Some(10);

    // Both arms return string
    res: string = match (val) {
        | Some(i) => i.to_string(), // i inferred as int
        | None => \"empty\"
    };
"
);

assert_types_err!(
    test_match_arm_mismatch,
    "
    type OptionInt = Some(int) | None;
    val = Some(10);

    match (val) {
        | Some(i) => i,         // Returns int
        | None => \"nothing\"   // Returns string -> Mismatch
    };
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_variant_constructor_arg_mismatch,
    "
    type Result = Ok(int) | Err(string);
    x = Ok(\"bad\"); // Ok expects int
",
    TypeError::TypeMismatch { .. }
);

// --- INTEGRATION TESTS? I GUESS SO ---

assert_types_ok!(
    test_complex_logic,
    "
    type User = { id: int, name: string };

    // Function taking list of records and returning map
    index_users(users: [User]): Map[int, string] = {
        mut m = Map();

        for u in users {
            m.insert(u.id, u.name);
        }

        m
    };

    // Data setup
    users = [
        { id: 1, name: \"Admin\" },
        { id: 2, name: \"Guest\" }
    ];

    // Execution
    lookup = index_users(users);
    name = lookup.get(1);
"
);

// --- FILE I/O & BUILTINS ---

assert_types_ok!(
    test_file_operations,
    "
    file = open(\"test.txt\", \"r\");
    content: string = file.read();
    file.close();
"
);

assert_types_err!(
    test_file_wrong_mode_type,
    "
    file = open(\"test.txt\", 123);
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_args_builtin,
    "
    filename = args.get(0);
    x: string = filename;
"
);

// --- STRING METHODS ---

assert_types_ok!(
    test_string_split_and_iteration,
    "
    line = \"hello,world\";
    parts = line.split(\",\");
    for part in parts {
        print(part);
    }
"
);

assert_types_ok!(
    test_string_char_at,
    "
    s = \"hello\";
    ch = s.char_at(0);
    x: string = ch;
"
);

assert_types_ok!(
    test_string_substring,
    "
    s = \"hello world\";
    sub = s.substring(0, 5);
    y: string = sub;
"
);

assert_types_ok!(
    test_string_parse_int,
    "
    s = \"123\";
    num = s.parse_int();
    x: int = num;
"
);

assert_types_ok!(
    test_string_trim,
    "
    s = \"  hello  \";
    trimmed = s.trim();
    x: string = trimmed;
"
);

assert_types_err!(
    test_parse_int_on_int,
    "
    x = 123;
    y = x.parse_int();
",
    TypeError::InvalidPropertyAccess { .. }
);

// --- LIST OPERATIONS ---

assert_types_ok!(
    test_list_push_reassignment,
    "
    mut digits: [int] = [];
    digits.push(5);
    digits.push(10);
"
);

assert_types_ok!(
    test_list_reverse,
    "
    mut list = [1, 2, 3];
    list = list.reverse();
"
);

assert_types_ok!(
    test_list_length,
    "
    list = [1, 2, 3];
    len = list.length();
    x: int = len;
"
);

assert_types_err!(
    test_list_push_type_mismatch_after_inference,
    "
    mut list = [1, 2, 3];
    list.push(\"string\");
",
    TypeError::TypeMismatch { .. }
);

// --- NESTED LISTS (2D GRIDS) ---

assert_types_ok!(
    test_nested_list_declaration,
    "
    banks: [[int]] = [];
    mut grid: [[int]] = [];
"
);

assert_types_ok!(
    test_nested_list_access,
    "
    grid = [[1, 2], [3, 4]];
    row = grid[0];
    val = grid[0][1];
    x: int = val;
"
);

assert_types_ok!(
    test_nested_list_building,
    "
    mut grid: [[int]] = [];
    row1 = [1, 2, 3];
    row2 = [4, 5, 6];
    grid.push(row1);
    grid.push(row2);

    m = grid.length();
    n = grid[0].length();
"
);

assert_types_err!(
    test_nested_list_type_mismatch,
    "
    grid: [[int]] = [[1, 2], [\"a\", \"b\"]];
",
    TypeError::TypeMismatch { .. }
);

// --- RANGE EXPRESSIONS ---

assert_types_ok!(
    test_range_exclusive,
    "
    for i in 0..<10 {
        print(i);
    }
"
);

assert_types_ok!(
    test_range_inclusive,
    "
    for i in 0..=10 {
        print(i);
    }
"
);

assert_types_err!(
    test_range_non_int,
    "
    for i in \"a\"..<\"z\" {
        print(i);
    }
",
    TypeError::TypeMismatch { .. }
);

assert_types_ok!(
    test_range_with_length,
    "
    list = [1, 2, 3, 4, 5];
    for i in 0..<list.length() {
        print(list[i]);
    }
"
);

// --- RECORDS & FIELD ACCESS ---

assert_types_ok!(
    test_record_type_definition,
    "
    type Range = {
        start: int,
        end: int
    };

    r: Range = {start: 0, end: 10};
    x: int = r.start;
    y: int = r.end;
"
);

assert_types_ok!(
    test_record_construction_inference,
    "
    point = {x: 10, y: 20};
    sum = point.x + point.y;
"
);

assert_types_err!(
    test_record_field_type_mismatch_construction,
    "
    type Range = {
        start: int,
        end: int
    };

    r: Range = {start: \"hello\", end: 10};
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_record_missing_field_access,
    "
    point = {x: 10, y: 20};
    z = point.z;
",
    TypeError::InvalidPropertyAccess { .. }
);

// --- FUNCTION SIGNATURES & RETURNS ---

assert_types_ok!(
    test_function_with_explicit_return_type,
    "
    get_number(): int = {
        42
    };

    x: int = get_number();
"
);

assert_types_ok!(
    test_function_implicit_return,
    "
    add(a: int, b: int): int = {
        a + b
    };

    result = add(5, 10);
    x: int = result;
"
);

assert_types_ok!(
    test_function_with_conditional_early_return,
    "
    abs(x: int): int = {
        if (x < 0) {
            return -x;
        };
        x
    };
"
);

assert_types_ok!(
    test_function_string_return,
    "
    get_direction(ch: string): int = {
        if (ch == \"L\") {
            return -1;
        };
        1
    };
"
);

// --- NESTED FUNCTIONS ---

assert_types_ok!(
    test_nested_function_definition,
    "
    outer(): int = {
        inner(x: int): int = {
            x * 2
        }
        inner(21)
    };
"
);

assert_types_ok!(
    test_closure_with_mutable_capture,
    "
    solve(): int = {
        mut res = 0;

        add_value(x: int) = {
            res = res + x;
        };

        add_value(5);
        add_value(10);
        res
    };
"
);

assert_types_ok!(
    test_nested_function_with_params,
    "
    process(): [[int]] = {
        grid = [[1, 0], [0, 1]];

        pass(g: [[int]]): [[int]] = {
            g
        };

        pass(grid)
    };
"
);

// --- CONTROL FLOW ---

assert_types_ok!(
    test_while_with_break,
    "
    mut i = 0;
    while (i < 10) {
        if (i == 5) {
            break;
        };
        i = i + 1;
    }
"
);

assert_types_ok!(
    test_for_with_continue,
    "
    mut sum = 0;
    for i in 0..=10 {
        if (i % 2 == 0) {
            continue;
        };
        sum = sum + i;
    }
"
);

assert_types_ok!(
    test_nested_loops,
    "
    for i in 0..<3 {
        for j in 0..<3 {
            print(i * 10 + j);
        }
    }
"
);

assert_types_ok!(
    test_nested_loop_with_grid,
    "
    grid = [[1, 2], [3, 4]];
    m = grid.length();
    n = grid[0].length();

    for i in 0..<m {
        for j in 0..<n {
            print(grid[i][j]);
        }
    }
"
);

// --- MODULO & ARITHMETIC ---

assert_types_ok!(
    test_modulo_arithmetic,
    "
    x = 100;
    y = x % 10;
    z: int = y;
"
);

assert_types_ok!(
    test_negative_modulo_wrap,
    "
    mut dial = 50;
    dial = (dial % 100 + 100) % 100;
"
);

assert_types_ok!(
    test_division_for_counting,
    "
    distance = 350;
    full_rotations = distance / 100;
    remainder = distance % 100;
"
);

// --- MUTABLE VARIABLES IN LOOPS ---

assert_types_ok!(
    test_mutable_accumulator,
    "
    mut total = 0;
    for i in 1..=100 {
        total = total + i;
    }
    print(total);
"
);

assert_types_ok!(
    test_mutable_list_building,
    "
    mut result: [int] = [];
    for i in 0..<5 {
        result.push(i * i);
    }
"
);

assert_types_ok!(
    test_mutable_grid_update,
    "
    mut grid = [[1, 0], [0, 1]];
    grid[0][0] = 0;
"
);

// --- COMPOUND EXPRESSIONS / METHOD CHAINING ---

assert_types_ok!(
    test_chained_method_calls,
    "
    s = \"  hello world  \";
    upper = s.trim().to_upper();
    parts = upper.split(\" \");
"
);

assert_types_ok!(
    test_string_to_int_conversion,
    "
    s = \"42\";
    num = s.parse_int();
    doubled = num * 2;
"
);

assert_types_ok!(
    test_int_to_string_conversion,
    "
    x = 42;
    s = x.to_string();
    len = s.length();
"
);

// --- COMPARISON OPERATORS ---

assert_types_ok!(
    test_comparison_chain,
    "
    x = 5;
    result = x >= 0 && x < 100;
"
);

assert_types_ok!(
    test_bounds_checking,
    "
    is_valid(x: int, y: int, m: int, n: int): bool = {
        x >= 0 && x < m && y >= 0 && y < n
    };
"
);

// --- MISC ---

assert_types_ok!(
    test_parse_line_pattern,
    "
    parse_line(line: string): [int] = {
        mut digits: [int] = [];
        chars = line.split(\"\");
        for ch in chars {
            if (ch == \".\") {
                digits.push(0);
            } else {
                digits.push(1);
            }
        }
        digits
    };
"
);

assert_types_ok!(
    test_file_processing_pattern,
    "
    get_lines(content: string): [[int]] = {
        lines = content.split(\"\\n\");
        mut result: [[int]] = [];

        for line in lines {
            trimmed = line.trim();
            if (trimmed.length() > 0) {
                mut row: [int] = [];
                row.push(1);
                result.push(row);
            }
        }
        result
    };
"
);

assert_types_ok!(
    test_grid_neighbor_check,
    "
    count_neighbors(x: int, y: int, grid: [[int]]): int = {
        m = grid.length();
        n = grid[0].length();
        mut count = 0;

        for dx in [-1, 0, 1] {
            for dy in [-1, 0, 1] {
                if (dx == 0 && dy == 0) {
                    continue;
                };
                nx = x + dx;
                ny = y + dy;
                if (nx >= 0 && nx < m && ny >= 0 && ny < n) {
                    if (grid[nx][ny] == 1) {
                        count = count + 1;
                    }
                }
            }
        }
        count
    };
"
);

assert_types_ok!(
    test_string_digit_parsing,
    "
    parse_digits(s: string): [int] = {
        mut result: [int] = [];
        chars = s.split(\"\");
        for ch in chars {
            if (ch != \"\") {
                digit = ch.parse_int();
                result.push(digit);
            }
        }
        result
    };
"
);

assert_types_ok!(
    test_accumulator_with_condition,
    "
    sum_valid(list: [int]): int = {
        mut sum = 0;
        for x in list {
            if (x > 0) {
                sum = sum + x;
            }
        }
        sum
    };
"
);

// --- ERROR CASES FROM REAL USAGE ---

assert_types_err!(
    test_cannot_mutate_immutable_accumulator,
    "
    result = 0;
    for i in 0..<10 {
        result = result + i;
    }
",
    TypeError::ImmutableAssignment(_)
);



assert_types_err!(
    test_wrong_return_type,
    "
    get_value(): int = {
        \"not an int\"
    };
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_list_index_wrong_type,
    "
    list = [1, 2, 3];
    x = list[\"0\"];
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_grid_access_wrong_types,
    "
    grid = [[1, 2], [3, 4]];
    x = grid[0.5][1];
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_comparison_type_mismatch,
    "
    result = 5 < \"10\";
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_arithmetic_type_mismatch,
    "
    result = 5 + \"hello\";
",
    TypeError::TypeMismatch { .. }
);

assert_types_err!(
    test_calling_non_function,
    "
    x = 42;
    result = x(10);
",
    TypeError::NotAFunction(_)
);

// --- EDGE CASES ---

assert_types_ok!(
    test_empty_list_with_annotation,
    "
    list: [int] = [];
"
);

assert_types_ok!(
    test_empty_string_check,
    "
    s = \"\";
    is_empty = s.length() == 0;
"
);

assert_types_ok!(
    test_negative_numbers,
    "
    x = -5;
    abs_x = if (x < 0) { -x } else { x };
"
);

assert_types_ok!(
    test_while_true_with_break,
    "
    mut i = 0;
    while (true) {
        i = i + 1;
        if (i > 10) {
            break;
        }
    }
"
);
