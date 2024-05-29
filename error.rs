use std::fmt::Debug;

use crate::lexer::{Token, TokenType};

pub fn lexer_error(line: u16, message: &str) {
    report(line, "", "Unexpected character");
}

pub fn error(token: Token, message: &str) {
    if token.token_type == TokenType::Eof {
        report(token.line, " at end", message);
    } else {
        let str = String::from("at ") + stringify!(token_type);
        report(token.line, str.as_str(), message);
    }
}
pub fn report(line: u16, place: &str, message: &str) {
    eprintln!("[line {}] Error {}: {}", line, place, message);
}
