use tap::ast::Span;
use tap::diagnostics::Reporter;
use tap::lexer::{Lexer, Token, TokenType};

#[test]
fn test_single_char_tokens() {
    let source = "+-*/;(){}[],.:"; // Added colon for completeness
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(TokenType::Plus, "+".to_string(), Span::new(0, 1)),
        Token::new(TokenType::Minus, "-".to_string(), Span::new(1, 2)),
        Token::new(TokenType::Star, "*".to_string(), Span::new(2, 3)),
        Token::new(TokenType::Slash, "/".to_string(), Span::new(3, 4)),
        Token::new(TokenType::Semicolon, ";".to_string(), Span::new(4, 5)),
        Token::new(TokenType::OpenParen, "(".to_string(), Span::new(5, 6)),
        Token::new(TokenType::CloseParen, ")".to_string(), Span::new(6, 7)),
        Token::new(TokenType::OpenBrace, "{".to_string(), Span::new(7, 8)),
        Token::new(TokenType::CloseBrace, "}".to_string(), Span::new(8, 9)),
        Token::new(TokenType::OpenBracket, "[".to_string(), Span::new(9, 10)),
        Token::new(TokenType::CloseBracket, "]".to_string(), Span::new(10, 11)),
        Token::new(TokenType::Comma, ",".to_string(), Span::new(11, 12)),
        Token::new(TokenType::Dot, ".".to_string(), Span::new(12, 13)),
        Token::new(TokenType::Colon, ":".to_string(), Span::new(13, 14)), // Added colon
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(14, 14)),
    ];

    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_multi_char_tokens() {
    let source = "== != <= >= && || += -= *= /= => :: !"; // Added '!' for Bang
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(TokenType::Equal, "==".to_string(), Span::new(0, 2)),
        Token::new(TokenType::NotEqual, "!=".to_string(), Span::new(3, 5)),
        Token::new(TokenType::LessThanEqual, "<=".to_string(), Span::new(6, 8)),
        Token::new(
            TokenType::GreaterThanEqual,
            ">=".to_string(),
            Span::new(9, 11),
        ),
        Token::new(TokenType::AmpAmp, "&&".to_string(), Span::new(12, 14)),
        Token::new(TokenType::PipePipe, "||".to_string(), Span::new(15, 17)),
        Token::new(TokenType::PlusEqual, "+=".to_string(), Span::new(18, 20)),
        Token::new(TokenType::MinusEqual, "-=".to_string(), Span::new(21, 23)),
        Token::new(TokenType::StarEqual, "*=".to_string(), Span::new(24, 26)),
        Token::new(TokenType::SlashEqual, "/=".to_string(), Span::new(27, 29)),
        Token::new(TokenType::FatArrow, "=>".to_string(), Span::new(30, 32)),
        Token::new(TokenType::DoubleColon, "::".to_string(), Span::new(33, 35)),
        Token::new(TokenType::Bang, "!".to_string(), Span::new(36, 37)),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(37, 37)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_keywords() {
    let source = "type mut if else while match true false None _";
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(TokenType::KeywordType, "type".to_string(), Span::new(0, 4)),
        Token::new(TokenType::KeywordMut, "mut".to_string(), Span::new(5, 8)),
        Token::new(TokenType::KeywordIf, "if".to_string(), Span::new(9, 11)),
        Token::new(
            TokenType::KeywordElse,
            "else".to_string(),
            Span::new(12, 16),
        ),
        Token::new(
            TokenType::KeywordWhile,
            "while".to_string(),
            Span::new(17, 22),
        ),
        Token::new(
            TokenType::KeywordMatch,
            "match".to_string(),
            Span::new(23, 28),
        ),
        Token::new(
            TokenType::KeywordTrue,
            "true".to_string(),
            Span::new(29, 33),
        ),
        Token::new(
            TokenType::KeywordFalse,
            "false".to_string(),
            Span::new(34, 39),
        ),
        Token::new(
            TokenType::KeywordNone,
            "None".to_string(),
            Span::new(40, 44),
        ),
        Token::new(
            TokenType::KeywordUnderscore,
            "_".to_string(),
            Span::new(45, 46),
        ),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(46, 46)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_integer_literals() {
    let source = "123 0 4567890";
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(TokenType::Integer(123), "123".to_string(), Span::new(0, 3)),
        Token::new(TokenType::Integer(0), "0".to_string(), Span::new(4, 5)),
        Token::new(
            TokenType::Integer(4567890),
            "4567890".to_string(),
            Span::new(6, 13),
        ),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(13, 13)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_float_literals() {
    let source = "123.45 0.0 987.654";
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(
            TokenType::Float(123.45),
            "123.45".to_string(),
            Span::new(0, 6),
        ),
        Token::new(TokenType::Float(0.0), "0.0".to_string(), Span::new(7, 10)),
        Token::new(
            TokenType::Float(987.654),
            "987.654".to_string(),
            Span::new(11, 18),
        ),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(18, 18)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_string_literals() {
    let source = r#""hello" "world 123" """#;
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(TokenType::String("hello".to_string()), "\"hello\"".to_string(), Span::new(0, 7)),
        Token::new(TokenType::String("world 123".to_string()), "\"world 123\"".to_string(), Span::new(8, 19)),
        Token::new(TokenType::String("".to_string()), "\"\"".to_string(), Span::new(20, 22)),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(22, 22)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_identifiers() {
    let source = "myVar another_var _underscore VAR123";
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(
            TokenType::Identifier("myVar".to_string()),
            "myVar".to_string(),
            Span::new(0, 5),
        ),
        Token::new(
            TokenType::Identifier("another_var".to_string()),
            "another_var".to_string(),
            Span::new(6, 17),
        ),
        Token::new(
            TokenType::Identifier("_underscore".to_string()),
            "_underscore".to_string(),
            Span::new(18, 29),
        ),
        Token::new(
            TokenType::Identifier("VAR123".to_string()),
            "VAR123".to_string(),
            Span::new(30, 36),
        ),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(36, 36)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_comments() {
    let source = "// This is a comment\n123 // Another comment\n456";
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(
            TokenType::Integer(123),
            "123".to_string(),
            Span::new(21, 24),
        ),
        Token::new(
            TokenType::Integer(456),
            "456".to_string(),
            Span::new(44, 47),
        ),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(47, 47)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_whitespace() {
    let source = "  \t123   +\n   456\r\n";
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(TokenType::Integer(123), "123".to_string(), Span::new(3, 6)),
        Token::new(TokenType::Plus, "+".to_string(), Span::new(9, 10)),
        Token::new(
            TokenType::Integer(456),
            "456".to_string(),
            Span::new(14, 17),
        ),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(19, 19)),
    ];
    assert_eq!(tokens, expected_tokens);
}

#[test]
fn test_mixed_tokens() {
    let source = r#"
type Point = { x: f64, y: f64 }; // Define a struct
mut counter = 0;
increment(c: int): int = {
    c = c + 1;
}
if counter < 10 && !false {
    increment(counter);
} else {
    // nothing
}
match counter {
    0 => "zero",
    _ => "not zero",
};
my_list = [1, 2, 3];
"hello world"
"#;
    let mut reporter = Reporter::new();
    let lexer = Lexer::new(source, &mut reporter);
    let tokens = lexer.tokenize().expect("Lexing failed");

    let expected_tokens = vec![
        Token::new(TokenType::KeywordType, "type".to_string(), Span::new(1, 5)),
        Token::new(TokenType::Identifier("Point".to_string()), "Point".to_string(), Span::new(6, 11)),
        Token::new(TokenType::Assign, "=".to_string(), Span::new(12, 13)),
        Token::new(TokenType::OpenBrace, "{".to_string(), Span::new(14, 15)),
        Token::new(TokenType::Identifier("x".to_string()), "x".to_string(), Span::new(16, 17)),
        Token::new(TokenType::Colon, ":".to_string(), Span::new(17, 18)),
        Token::new(TokenType::Identifier("f64".to_string()), "f64".to_string(), Span::new(19, 22)),
        Token::new(TokenType::Comma, ",".to_string(), Span::new(22, 23)),
        Token::new(TokenType::Identifier("y".to_string()), "y".to_string(), Span::new(24, 25)),
        Token::new(TokenType::Colon, ":".to_string(), Span::new(25, 26)),
        Token::new(TokenType::Identifier("f64".to_string()), "f64".to_string(), Span::new(27, 30)),
        Token::new(TokenType::CloseBrace, "}".to_string(), Span::new(31, 32)),
        Token::new(TokenType::Semicolon, ";".to_string(), Span::new(32, 33)),
        Token::new(TokenType::KeywordMut, "mut".to_string(), Span::new(53, 56)),
        Token::new(TokenType::Identifier("counter".to_string()), "counter".to_string(), Span::new(57, 64)),
        Token::new(TokenType::Assign, "=".to_string(), Span::new(65, 66)),
        Token::new(TokenType::Integer(0), "0".to_string(), Span::new(67, 68)),
        Token::new(TokenType::Semicolon, ";".to_string(), Span::new(68, 69)),
        Token::new(TokenType::Identifier("increment".to_string()), "increment".to_string(), Span::new(70, 79)),
        Token::new(TokenType::OpenParen, "(".to_string(), Span::new(79, 80)),
        Token::new(TokenType::Identifier("c".to_string()), "c".to_string(), Span::new(80, 81)),
        Token::new(TokenType::Colon, ":".to_string(), Span::new(81, 82)),
        Token::new(TokenType::Identifier("int".to_string()), "int".to_string(), Span::new(83, 86)),
        Token::new(TokenType::CloseParen, ")".to_string(), Span::new(86, 87)),
        Token::new(TokenType::Colon, ":".to_string(), Span::new(87, 88)),
        Token::new(TokenType::Identifier("int".to_string()), "int".to_string(), Span::new(89, 92)),
        Token::new(TokenType::Assign, "=".to_string(), Span::new(93, 94)),
        Token::new(TokenType::OpenBrace, "{".to_string(), Span::new(95, 96)),
        Token::new(TokenType::Identifier("c".to_string()), "c".to_string(), Span::new(101, 102)),
        Token::new(TokenType::Assign, "=".to_string(), Span::new(103, 104)),
        Token::new(TokenType::Identifier("c".to_string()), "c".to_string(), Span::new(105, 106)),
        Token::new(TokenType::Plus, "+".to_string(), Span::new(107, 108)),
        Token::new(TokenType::Integer(1), "1".to_string(), Span::new(109, 110)),
        Token::new(TokenType::Semicolon, ";".to_string(), Span::new(110, 111)),
        Token::new(TokenType::CloseBrace, "}".to_string(), Span::new(112, 113)),
        Token::new(TokenType::KeywordIf, "if".to_string(), Span::new(114, 116)),
        Token::new(TokenType::Identifier("counter".to_string()), "counter".to_string(), Span::new(117, 124)),
        Token::new(TokenType::LessThan, "<".to_string(), Span::new(125, 126)),
        Token::new(TokenType::Integer(10), "10".to_string(), Span::new(127, 129)),
        Token::new(TokenType::AmpAmp, "&&".to_string(), Span::new(130, 132)),
        Token::new(TokenType::Bang, "!".to_string(), Span::new(133, 134)),
        Token::new(TokenType::KeywordFalse, "false".to_string(), Span::new(134, 139)),
        Token::new(TokenType::OpenBrace, "{".to_string(), Span::new(140, 141)),
        Token::new(TokenType::Identifier("increment".to_string()), "increment".to_string(), Span::new(146, 155)),
        Token::new(TokenType::OpenParen, "(".to_string(), Span::new(155, 156)),
        Token::new(TokenType::Identifier("counter".to_string()), "counter".to_string(), Span::new(156, 163)),
        Token::new(TokenType::CloseParen, ")".to_string(), Span::new(163, 164)),
        Token::new(TokenType::Semicolon, ";".to_string(), Span::new(164, 165)),
        Token::new(TokenType::CloseBrace, "}".to_string(), Span::new(166, 167)),
        Token::new(TokenType::KeywordElse, "else".to_string(), Span::new(168, 172)),
        Token::new(TokenType::OpenBrace, "{".to_string(), Span::new(173, 174)),
        Token::new(TokenType::CloseBrace, "}".to_string(), Span::new(190, 191)),
        Token::new(TokenType::KeywordMatch, "match".to_string(), Span::new(192, 197)),
        Token::new(TokenType::Identifier("counter".to_string()), "counter".to_string(), Span::new(198, 205)),
        Token::new(TokenType::OpenBrace, "{".to_string(), Span::new(206, 207)),
        Token::new(TokenType::Integer(0), "0".to_string(), Span::new(212, 213)),
        Token::new(TokenType::FatArrow, "=>".to_string(), Span::new(214, 216)),
        Token::new(TokenType::String("zero".to_string()), "\"zero\"".to_string(), Span::new(217, 223)),
        Token::new(TokenType::Comma, ",".to_string(), Span::new(223, 224)),
        Token::new(TokenType::KeywordUnderscore, "_".to_string(), Span::new(229, 230)),
        Token::new(TokenType::FatArrow, "=>".to_string(), Span::new(231, 233)),
        Token::new(TokenType::String("not zero".to_string()), "\"not zero\"".to_string(), Span::new(234, 244)),
        Token::new(TokenType::Comma, ",".to_string(), Span::new(244, 245)),
        Token::new(TokenType::CloseBrace, "}".to_string(), Span::new(246, 247)),
        Token::new(TokenType::Semicolon, ";".to_string(), Span::new(247, 248)),
        Token::new(TokenType::Identifier("my_list".to_string()), "my_list".to_string(), Span::new(249, 256)),
        Token::new(TokenType::Assign, "=".to_string(), Span::new(257, 258)),
        Token::new(TokenType::OpenBracket, "[".to_string(), Span::new(259, 260)),
        Token::new(TokenType::Integer(1), "1".to_string(), Span::new(260, 261)),
        Token::new(TokenType::Comma, ",".to_string(), Span::new(261, 262)),
        Token::new(TokenType::Integer(2), "2".to_string(), Span::new(263, 264)),
        Token::new(TokenType::Comma, ",".to_string(), Span::new(264, 265)),
        Token::new(TokenType::Integer(3), "3".to_string(), Span::new(266, 267)),
        Token::new(TokenType::CloseBracket, "]".to_string(), Span::new(267, 268)),
        Token::new(TokenType::Semicolon, ";".to_string(), Span::new(268, 269)),
        Token::new(TokenType::String("hello world".to_string()), "\"hello world\"".to_string(), Span::new(270, 283)),
        Token::new(TokenType::EndOfFile, "".to_string(), Span::new(284, 284)),
    ];
    assert_eq!(tokens, expected_tokens);
}
