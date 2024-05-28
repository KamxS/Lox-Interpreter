use core::fmt;
use std::{fs};

pub mod lexer;
pub mod error;
pub mod parser;
use lexer::{Lexer,Token, TokenType};
use parser::{Parser};

fn run(src: &String) {
    let mut lex = Lexer::new(src);
    lex.scan();
    let mut parser = Parser::new(lex.get_tokens());
    println!("{}",parser.expression());
}

fn main() {
    let contents = fs::read_to_string("test")
        .expect("Could not open a file");
    run(&contents);
}
