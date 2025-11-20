use std::cell::RefCell;
use std::rc::Rc;
use tap::environment::Environment;
use tap::interpreter::{Interpreter, Value};
use tap::lexer::Lexer;
use tap::parser::Parser;

/// Helper function to run source code and return the final value.
fn run_tap(source: &str) -> Option<Value> {
    let tokens = Lexer::new(source).tokenize().expect("Lexer error");

    let mut parser = Parser::new(&tokens, source);
    let program = parser.parse_program().expect("Parser error");

    let interpreter = Interpreter::new();
    let env = Rc::new(RefCell::new(Environment::new()));

    interpreter.interpret(&program, env).expect("Runtime error")
}

#[test]
fn test_structs() {
    // file: structs.tap
    let source = r#"
        type Rect = struct {
            width: int,
            height: int
        };

        func area(r: Rect) -> int {
            r.width * r.height;
        }

        my_rect : Rect = Rect { width: 10, height: 5 };

        # Modify a property
        my_rect.width = 20;

        area(my_rect); # Should return 100
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(100)));
}

#[test]
fn test_option_type() {
    // file: option_type.tap
    let source = r#"
        type MaybeInt = enum { Some(int), None };

        func safe_div(a: int, b: int) -> MaybeInt {
            if b == 0 {
                MaybeInt::None
            } else {
                MaybeInt::Some(a / b)
            }
        }

        # Pattern match to unwrap the value
        match safe_div(10, 2) {
            Some(val) => val,
            None => 0
        };
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(5)));
}

#[test]
fn test_iterative_fib() {
    // file: iterative_fib.tap
    let source = r#"
        func fib_iter(n: int) -> int {
            a : int = 0;
            b : int = 1;
            count : int = 0;

            while count < n {
                temp : int = a;
                a = b;
                b = temp + b;
                count = count + 1;
            }
            a;
        }

        fib_iter(10); # Should return 55
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(55)));
}

#[test]
fn test_conditional_as_value() {
    // file: conditional_as_value.tap
    let source = r#"
        age : int = 20;

        # Assigning the result of an if-block to a variable
        status : str = if age >= 18 {
            "Adult"
        } else {
            "Minor"
        };

        status == "Adult"
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_fib_lambda() {
    // file: fib_lambda.tap
    let source = r#"
        # comment tokens get removed at lexing time, up until the new line

        # inline function definition of fibonacci
        fib : int -> int =
            \x. (
                if x < 2 {
                    x;
                } else {
                    fib(x - 1) + fib(x - 2);
                }
            );

        fib(5) == 5;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_lambdas2() {
    // file: lambdas2.tap
    let source = r#"
        # A function that takes an int and a function(int -> int)
        func transform(val: int, op: int -> int) -> int {
            op(val);
        }

        # Pass a lambda that squares the input
        transform(5, \x. x * x);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(25)));
}

#[test]
fn test_enum_decl() {
    // file: enum.tap
    let source = r#"
        Color : enum { Red, Green, Blue };
    "#;
    let result = run_tap(source);
    // Declarations return Unit, which interpreter converts to None
    assert_eq!(result, None);
}

#[test]
fn test_inline_lambda() {
    // file: inline_lambda.tap
    let source = r#"
        res = (\y. y + 10)(10);  # Expected output: 20
        res == 20;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_list_sum() {
    // file: list_sum.tap
    let source = r#"
        numbers : [int] = [10, 20, 30, 40, 50];
        total : int = 0;

        for n in numbers {
            total = total + n;
        }

        total;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(150)));
}

#[test]
fn test_fib() {
    // file: fib.tap
    let source = r#"
        func fib(n) {
            if n < 2 {
                return n;
            }
            return fib(n - 1) + fib(n - 2);
        }

        fib(5) == 5;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_lambda_declaration() {
    // file: lambda.tap
    let source = r#"
        inc: int -> int = \x. x + 1;

        inc(5) == 6;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_hello() {
    // file: hello.tap
    let source = r#"
        func main() {
            "Hello, World!";
        }

        main();
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::String("Hello, World!".to_string())));
}

#[test]
fn test_array_mutation() {
    // file: array_mutation.tap
    let source = r#"
        matrix : [int] = [0, 0, 0];

        matrix[0] = 1;
        matrix[1] = 2;
        matrix[2] = 3;

        # Should return [1, 2, 3]
        matrix;
    "#;
    let result = run_tap(source);
    assert_eq!(
        result,
        Some(Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3)
        ]))
    );
}

#[test]
fn test_match() {
    // file: match.tap
    let source = r#"
        type Light = enum { Red, Yellow, Green };

        func action(l: Light) -> str {
            match l {
                Red => "Stop",
                Yellow => "Caution",
                Green => "Go"
            }
        }

        action(Light::Yellow);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::String("Caution".to_string())));
}

#[test]
fn test_dodaj_polish() {
    // file: dodaj.tap
    let source = r#"
        funkcja dodaj(a, b) {
            zwróć a + b;
        }

        dodaj(4, 5) == 9;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_hi_func() {
    // file: hi_func.tap
    let source = r#"
        func multiply(a, b) {
            return a * b;
        }

        double: int -> int;
        double = \x. multiply(x, 2);
        double(6) == 12;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_silnia_polish() {
    // file: silnia.tap
    let source = r#"
        funkcja silnia(n)  {
            jeśli n == 0 {
                zwróć 1;
            }
            zwróć n * silnia(n - 1);
        }

        silnia(5) == 120;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_float_arithmetic() {
    let source = r#"
        a : float = 10.5;
        b : float = 2.5;
        a + b;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Float(13.0)));
}

#[test]
fn test_float_comparison() {
    let source = r#"
        1.5 < 2.5;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_string_concatenation() {
    let source = r#"
        "Hello" + " " + "World";
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::String("Hello World".to_string())));
}

#[test]
fn test_logic_short_circuit_and() {
    // If short-circuiting works, the division by zero won't happen
    let source = r#"
        false && (1 / 0 == 0);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(false)));
}

#[test]
fn test_logic_short_circuit_or() {
    // If short-circuiting works, the division by zero won't happen
    let source = r#"
        true || (1 / 0 == 0);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_unary_operators() {
    let source = r#"
        x : int = 10;
        flag : bool = true;
        (-x == -10) && (!flag == false);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_modulo() {
    let source = r#"
        10 % 3;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(1)));
}

#[test]
fn test_precedence_order() {
    let source = r#"
        # Should be 2 + (3 * 4) = 14, not (2 + 3) * 4 = 20
        2 + 3 * 4;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(14)));
}

#[test]
fn test_nested_loops_break() {
    let source = r#"
        outer : int = 0;
        inner : int = 0;
        sum : int = 0;

        while outer < 3 {
            inner = 0;
            while inner < 3 {
                if inner == 1 {
                    break;
                }
                sum = sum + 1;
                inner = inner + 1;
            }
            outer = outer + 1;
        }
        sum;
    "#;
    // Outer runs 3 times. Inner runs: 0 (sum+1), 1 (break).
    // So sum increments once per outer loop. Total 3.
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(3)));
}

#[test]
fn test_nested_loops_continue() {
    let source = r#"
        i : int = 0;
        sum : int = 0;
        while i < 5 {
            i = i + 1;
            if i == 3 {
                continue;
            }
            sum = sum + i;
        }
        sum;
    "#;
    // i=1, sum=1
    // i=2, sum=3
    // i=3, continue
    // i=4, sum=7
    // i=5, sum=12
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(12)));
}

#[test]
fn test_mutual_recursion() {
    let source = r#"
        func is_even(n: int) -> bool {
            if n == 0 {
                true
            } else {
                is_odd(n - 1)
            }
        }

        func is_odd(n: int) -> bool {
            if n == 0 {
                false
            } else {
                is_even(n - 1)
            }
        }

        is_even(4) && is_odd(3);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}

#[test]
fn test_nested_if_expression() {
    let source = r#"
        x : int = 10;
        y : int = 20;

        res : str = if x > 5 {
            if y < 10 {
                "A"
            } else {
                "B"
            }
        } else {
            "C"
        };
        res;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::String("B".to_string())));
}

#[test]
#[ignore]
fn test_polish_while_loop() {
    let source = r#"
        licznik : int = 0;
        dopóki licznik < 5 {
            licznik = licznik + 1;
        }
        licznik;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(5)));
}

#[test]
fn test_polish_if_else() {
    let source = r#"
        x : int = 10;
        wynik : int = 0;
        jeśli x > 100 {
            wynik = 1;
        } inaczej {
            wynik = 2;
        }
        wynik;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(2)));
}

#[test]
fn test_array_access_expression() {
    let source = r#"
        [10, 20, 30][1];
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(20)));
}

#[test]
fn test_function_returning_lambda() {
    let source = r#"
        func make_adder(n: int) -> int -> int {
            \x. x + n;
        }

        add5 = make_adder(5);
        add5(10);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(15)));
}

#[test]
fn test_struct_access_nested() {
    let source = r#"
        type Point = struct { x: int, y: int };
        type Line = struct { start: Point, end: Point };

        l : Line = Line {
            start: Point { x: 0, y: 0 },
            end: Point { x: 10, y: 20 }
        };

        l.end.y;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(20)));
}

#[test]
fn test_early_return_in_loop() {
    let source = r#"
        func find_match(target: int, list: [int]) -> int {
            for x in list {
                if x == target {
                    return x;
                }
            }
            return -1;
        }

        find_match(30, [10, 20, 30, 40]);
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Integer(30)));
}

#[test]
fn test_unit_return() {
    // Empty block returns Unit (None in run_tap if implicit return)
    // But we can verify assignment of Unit
    let source = r#"
        x = {};
        x;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Unit));
}

#[test]
fn test_complex_boolean_logic() {
    let source = r#"
        a : bool = true;
        b : bool = false;
        c : bool = true;

        # (T || F) && T -> T && T -> T
        (a || b) && c;
    "#;
    let result = run_tap(source);
    assert_eq!(result, Some(Value::Boolean(true)));
}
