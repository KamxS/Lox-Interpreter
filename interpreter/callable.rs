use std::fmt;
use std::collections::HashMap;
use std::error::Error;

use crate::interpreter::{Interpreter, ReturnError, RuntimeError, Value};
use crate::lexer::TokenType;
use crate::parser::Stmt;

#[derive(Clone, PartialEq, Debug)]
pub enum Callable {
    Native(NativeCallable),
    Lox(LoxCallable)
}

#[derive(Clone)]
pub struct NativeCallable {
    pub arity: usize,
    call_f: fn(&Interpreter, Vec<Value>)->Value
}
impl fmt::Debug for NativeCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Point")
         .field("arity", &self.arity)
         .finish()
    }
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
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

#[derive(Clone, Debug)]
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
    pub fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, Box<dyn Error>>{
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
                let exec_result = interpreter.execute(stmt);
                if exec_result.is_err() {
                    interpreter.vars.pop();
                    let err = exec_result.unwrap_err();
                    if let Some(r_err) = err.downcast_ref::<ReturnError>() {
                        return Ok(r_err.value.clone());
                    }
                    return Err(err);
                }
            }
            interpreter.vars.pop();
            return Ok(Value::Null);
        }
        panic!()
    }
}
impl PartialEq for LoxCallable {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}
