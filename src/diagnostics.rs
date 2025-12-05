use crate::ast::Span;

/// Represents the severity of a diagnostic message.
#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticKind {
    Error,
    Warning,
    // Info, // Potentially add info or hint
}

/// Represents a single diagnostic message (error, warning, etc.).
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    pub kind: DiagnosticKind,
    pub message: String,
    pub span: Span,
    pub context: Option<String>,
}

impl Diagnostic {
    pub fn new(kind: DiagnosticKind, message: String, span: Span) -> Self {
        Diagnostic {
            kind,
            message,
            span,
            context: None,
        }
    }

    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }
}

/// Manages and collects diagnostic messages during lexing, parsing, and other phases.
pub struct Reporter {
    pub diagnostics: Vec<Diagnostic>,
    pub has_errors: bool,
}

impl Reporter {
    pub fn new() -> Self {
        Reporter {
            diagnostics: Vec::new(),
            has_errors: false,
        }
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        if diagnostic.kind == DiagnosticKind::Error {
            self.has_errors = true;
        }
        self.diagnostics.push(diagnostic);
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }

    pub fn format_diagnostics(&self, source: &str) -> String {
        let mut output = String::new();
        for diagnostic in &self.diagnostics {
            let (line, col) = byte_to_line_col(source, diagnostic.span.start);
            let kind_str = match diagnostic.kind {
                DiagnosticKind::Error => "Error",
                DiagnosticKind::Warning => "Warning",
            };

            output.push_str(&format!("{} at line {}, column {}", kind_str, line, col));
            if let Some(context) = &diagnostic.context {
                output.push_str(&format!(" ({})", context));
            }
            output.push_str(&format!(": {}\n", diagnostic.message));

            // Show the line with the error
            if let Some(line_text) = get_line(source, line) {
                output.push_str(&format!("  | {}\n", line_text));
                output.push_str(&format!("  | {}^\n", " ".repeat(col.saturating_sub(1))));
            }
        }
        output
    }

    /// Reports all collected diagnostics to stderr.
    /// In a real compiler, this would involve pretty-printing with source context.
    pub fn emit_diagnostics(&self, _source: &str) {
        for diagnostic in &self.diagnostics {
            // For now, simple printing. This will be expanded later for pretty-printing.
            eprintln!(
                "{:?} at {:?} (Context: {:?}): {}",
                diagnostic.kind, diagnostic.span, diagnostic.context, diagnostic.message
            );
        }
    }
}

// --- Utility functions for rich error reporting (to be used by format_diagnostic later) ---

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
pub fn get_line(source: &str, line_num: usize) -> Option<&str> {
    source.lines().nth(line_num - 1)
}
