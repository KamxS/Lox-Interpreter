use std::collections::HashMap;

use crate::interpreter::{Interpreter, Value};
use crate::lexer::{Token, TokenType};
use crate::parser::Stmt;

#[derive(Clone, PartialEq)]
pub enum Callable {
    Native(NativeCallable),
    Lox(LoxCallable)
}

#[derive(Clone)]
pub struct NativeCallable {
    pub arity: usize,
    call_f: fn(&Interpreter, Vec<Value>)->Value
}
impl NativeCallable {
    pub fn new(arity:usize,call_f: fn(&Interpreter, Vec<Value>)-> Value) -> Self {
        Self{arity, call_f}
    }
    pub fn call(&self, interpreter: &Interpreter, args: Vec<Value>) -> Value{
        (self.call_f)(interpreter, args)
    }
}
impl PartialEq for NativeCallable {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}

#[derive(Clone)]
pub struct LoxCallable {
    pub arity: usize,
    definition: Stmt
}
impl LoxCallable {
    pub fn new(definition: Stmt) -> Self {
        if let Stmt::Func(_, params, _) = &definition {
            return Self {
                arity: params.len(),
                definition
            };
        }
        panic!()
    }
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) {
        if let Stmt::Func(_, params, block) = &self.definition {
            let mut local = HashMap::new();
            for ind in 0..params.len() {
                if let TokenType::Id(name) = &params[ind].token_type {
                    local.insert(name.clone(), args[ind].clone());
                }else {
                    panic!()
                }
            }
            interpreter.vars.push(local);
            for stmt in block {
                interpreter.execute(stmt);
            }
            return;
        }
        panic!()
    }
}
impl PartialEq for LoxCallable {
    fn eq(&self, other: &Self) -> bool {
        false
    }
}
