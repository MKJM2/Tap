use crate::parser::ParseError;
use std::fmt::Write;

/// Convert byte offset to (line, column)
pub fn byte_to_line_col(source: &str, byte_offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    for (idx, ch) in source.char_indices() {
        if idx >= byte_offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

/// Extract a specific line from source
fn get_line(source: &str, line_num: usize) -> Option<&str> {
    source.lines().nth(line_num - 1)
}

/// Format a diagnostic error with pretty-printing
pub fn format_diagnostic(error: &ParseError, source: &str, filename: &str) -> String {
    let mut output = String::new();

    match error {
        ParseError::UnexpectedToken {
            context,
            expected,
            found,
            span,
            ..
        } => {
            let (line, col) = byte_to_line_col(source, span.lo);

            // Header
            writeln!(
                &mut output,
                "\x1b[1;31merror\x1b[0m: Unexpected token in {}",
                context
            )
            .unwrap();
            writeln!(
                &mut output,
                "  \x1b[1;34m-->\x1b[0m {}:{}:{}",
                filename, line, col
            )
            .unwrap();

            // Context lines
            if let Some(line_content) = get_line(source, line) {
                writeln!(&mut output, "\x1b[1;34m{:>4} |\x1b[0m", line).unwrap();
                writeln!(&mut output, "\x1b[1;34m     |\x1b[0m {}", line_content).unwrap();

                // Underline with carets
                let underline_start = col - 1;
                let underline_len = span.len().max(1);
                let spaces = " ".repeat(underline_start);
                let carets = "\x1b[1;31m".to_string() + &"^".repeat(underline_len) + "\x1b[0m";

                writeln!(
                    &mut output,
                    "\x1b[1;34m     |\x1b[0m {}{} expected {}, found '{}'",
                    spaces, carets, expected, found
                )
                .unwrap();
            }
        }

        ParseError::UnexpectedEof { context, span } => {
            let (line, col) = byte_to_line_col(source, span.lo);

            writeln!(
                &mut output,
                "\x1b[1;31merror\x1b[0m: Unexpected end of file"
            )
            .unwrap();
            writeln!(
                &mut output,
                "  \x1b[1;34m-->\x1b[0m {}:{}:{}",
                filename, line, col
            )
            .unwrap();
            writeln!(&mut output, "\x1b[1;34m     |\x1b[0m").unwrap();
            writeln!(
                &mut output,
                "\x1b[1;34m     |\x1b[0m \x1b[1;31m^\x1b[0m unexpected EOF while parsing {}",
                context
            )
            .unwrap();
        }
    }

    output
}

/// Check if ANSI colors should be disabled
pub fn should_use_colors() -> bool {
    // Check if output is a TTY and TERM is set
    std::env::var("NO_COLOR").is_err() && atty::is(atty::Stream::Stderr)
}

/// Format diagnostic without colors
pub fn format_diagnostic_plain(error: &ParseError, source: &str, filename: &str) -> String {
    // Similar to above but without ANSI codes
    let mut output = String::new();

    match error {
        ParseError::UnexpectedToken {
            context,
            expected,
            found,
            span,
            ..
        } => {
            let (line, col) = byte_to_line_col(source, span.lo);
            writeln!(&mut output, "error: Unexpected token in {}", context).unwrap();
            writeln!(&mut output, "  --> {}:{}:{}", filename, line, col).unwrap();

            if let Some(line_content) = get_line(source, line) {
                writeln!(&mut output, "{:>4} |", line).unwrap();
                writeln!(&mut output, "     | {}", line_content).unwrap();

                let underline_start = col - 1;
                let underline_len = span.len().max(1);
                let spaces = " ".repeat(underline_start);
                let carets = "^".repeat(underline_len);

                writeln!(
                    &mut output,
                    "     | {}{} expected {}, found '{}'",
                    spaces, carets, expected, found
                )
                .unwrap();
            }
        }

        ParseError::UnexpectedEof { context, span } => {
            let (line, col) = byte_to_line_col(source, span.lo);
            writeln!(&mut output, "error: Unexpected end of file").unwrap();
            writeln!(&mut output, "  --> {}:{}:{}", filename, line, col).unwrap();
            writeln!(&mut output, "     |").unwrap();
            writeln!(
                &mut output,
                "     | ^ unexpected EOF while parsing {}",
                context
            )
            .unwrap();
        }
    }

    output
}
