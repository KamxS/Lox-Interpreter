use crate::lexer::{Token, TokenType};

pub fn lexer_error(line: u16, message: &'static str) {
    report(line, "", message);
}

pub fn error(token: &Token, message: &'static str) {
    if token.token_type == TokenType::Eof {
        report(token.line, " at end", message);
    } else {
        let str = String::from("at ") + token.token_type.to_string().as_str();
        report(token.line, str.as_str(), message);
    }
}
pub fn report(line: u16, place: &str, message: &str) {
    eprintln!("[line {}] Error {}: {}", line, place, message);
}
