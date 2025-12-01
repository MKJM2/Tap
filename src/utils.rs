use crate::lexer::Token;

pub fn pretty_print_tokens(tokens: &[Token]) -> String {
    tokens
        .iter()
        .map(|t| format!("{}", t))
        .collect::<Vec<_>>()
        .join(", ")
}
