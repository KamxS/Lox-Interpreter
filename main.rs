use std::{fs};

pub mod error;
pub mod lexer;
pub mod parser;
pub mod interpreter;
use lexer::{Lexer};
use parser::{Parser};
use interpreter::{Interpreter};

fn run(src: &String) {
    let mut lex = Lexer::new(src);
    lex.scan();
    let mut parser = Parser::new(lex.get_tokens());
    let stmts = parser.parse();
    /*
    for stmt in stmts.iter() {
        println!("{:?}", stmt);
    }
    */
    let mut interpreter = Interpreter::new();
    interpreter.interpret(&stmts);
}

fn main() {
    let contents = fs::read_to_string("test")
        .expect("Could not open a file");
    run(&contents);
}
