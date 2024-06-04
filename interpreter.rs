use core::{fmt, panic};
use std::collections::HashMap;

use crate::parser::{Expr, Stmt};
use crate::lexer::{Token,TokenType};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String), 
    Number(f32), 
    Bool(bool),
    Null
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Null => write!(f, "Null")
        }
    }
}
#[derive(Debug, Clone)]
struct RuntimeError {
    token: Token,
    message: &'static str
}
impl RuntimeError {
    fn new(token: &Token, message: &'static str) -> Self {
        Self { 
            token:token.clone(), 
            message
        }
    }
}
impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f,"[line {}] {}", self.token.line, self.message);
    }
}

pub struct Interpreter {
    vars: HashMap<String, Value>
}
impl Interpreter {
    pub fn new() -> Self {
        Self{vars: HashMap::new()}
    }
    pub fn interpret(&mut self, statements: &Vec<Stmt>) {
        for stmt in statements.iter() {
            self.execute(stmt);
        }
    }

    fn execute(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Print(_) => self.print_stmt(stmt),
            Stmt::Var(_, _) => self.var_decl_stmt(stmt),
            _ => self.expr_stmt(stmt)
        }
    }

    fn var_decl_stmt(&mut self, stmt: &Stmt) {
        if let Stmt::Var(token, expr) = stmt {
            let mut val = Value::Null;
            if let Some(e) = expr {
                val = self.eval(e).unwrap();
            }
            self.define(token, val);
        }
    }

    fn print_stmt(&mut self, stmt: &Stmt) {
        if let Stmt::Print(expr)= stmt{
            println!("{}", self.eval(expr).unwrap());
        }
    }

    fn expr_stmt(&mut self, stmt: &Stmt) {
        if let Stmt::Expression(expr)= stmt{
            self.eval(expr);
        }
    }

    fn eval(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Literal(_) => Ok(self.eval_literal(expr)),
            Expr::Grouping(e) => self.eval(e),
            Expr::Unary(_,_) => self.eval_unary(expr),
            Expr::Binary(_,_,_) => self.eval_binary(expr),
            Expr::Variable(_) => self.eval_variable(expr),
        }
    }

    fn eval_literal(&self, expr: &Expr) -> Value{
        if let Expr::Literal(l) = expr {
            match &l.token_type {
                TokenType::String(s) => return Value::String(s.clone()),
                TokenType::Number(n) => return Value::Number(n.clone()),
                TokenType::False => return Value::Bool(false),
                TokenType::True => return Value::Bool(true),
                TokenType::Nil => return Value::Null,
                _ => panic!("Invalid token: {:?}", l)
            }
        }
        Value::Null
    }

    fn eval_variable(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        if let Expr::Variable(name) = expr{
            return self.get(name);
        }
        panic!("Not really possible i guess");
    }

    fn eval_unary(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        if let Expr::Unary(t, expr) = expr {
            let value = self.eval(expr)?;
            match t.token_type {
                TokenType::Minus => {
                    return Ok(Value::Number(-self.get_num(&value)));
                },
                TokenType::Bang=> {
                    return Ok(Value::Bool(!self.get_bool(&value)));
                },
                _ => panic!("[parser_error] This token can't be a part of unary expression: {:?}", t)
            }
        }
        Ok(Value::Null)
    }

    fn eval_binary(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        if let Expr::Binary(l_expr, t, r_expr) = expr {
            let l_value = self.eval(l_expr)?;
            let r_value = self.eval(r_expr)?;
            match t.token_type {
                TokenType::Minus => return Ok(Value::Number(self.get_num(&l_value)-self.get_num(&r_value))),
                TokenType::Star=> return Ok(Value::Number(self.get_num(&l_value)*self.get_num(&r_value))),
                TokenType::Slash => {
                    let r_num = self.get_num(&r_value);
                    if r_num == 0.0 {
                        panic!("Division by 0 is forbidden");
                    }
                    return Ok(Value::Number(self.get_num(&l_value)/r_num));
                },
                TokenType::Plus => {
                    if let Value::String(l_str) = l_value {
                        match r_value {
                            Value::String(r_str) => return Ok(Value::String(l_str + r_str.as_str())),
                            Value::Number(r_num) => return Ok(Value::String(l_str + r_num.to_string().as_str())),
                            _ => panic!("Following type can't be concatinated with a string")
                        }
                    }else {
                        let l_num = self.get_num(&l_value);
                        match r_value {
                            Value::String(r_str) => return Ok(Value::String(l_num.to_string() + r_str.as_str())),
                            Value::Number(r_num) => return Ok(Value::Number(l_num + r_num)),
                            _ => panic!("Expected expected a number or a string")
                        }
                    }
                }
                TokenType::Greater => return Ok(Value::Bool(self.get_num(&l_value)>self.get_num(&r_value))),
                TokenType::GreaterEqual => return Ok(Value::Bool(self.get_num(&l_value)>=self.get_num(&r_value))),
                TokenType::Less => return Ok(Value::Bool(self.get_num(&l_value)<self.get_num(&r_value))),
                TokenType::LessEqual => return Ok(Value::Bool(self.get_num(&l_value)<=self.get_num(&r_value))),
                TokenType::EqualEqual => return Ok(Value::Bool(self.is_equal(&l_value, &r_value))),
                TokenType::BangEqual => return Ok(Value::Bool(!self.is_equal(&l_value, &r_value))),
                _ => {}
            }
        }
        Ok(Value::Null)
    }

    fn get_bool(&self, value: &Value) -> bool {
        match value {
            Value::Bool(b) => *b,
            Value::Null => false,
            _ => true
        }
    }

    fn get_num(&self, value: &Value) -> f32 {
        if let Value::Number(n) = value {
            return *n;
        }else {
            panic!("Expected a number");
        }
    }

    fn is_equal(&self, value_l: &Value, value_r: &Value) -> bool {
        *value_l == *value_r
    }

    fn define(&mut self, token: &Token, value: Value) {
        match &token.token_type {
            TokenType::Identifier(name) => {
                self.vars.insert(name.clone(), value);
                return;
            }
            _ => panic!("This shouldn't be here (parser error)")
        }
    }

    fn get(&mut self, token: &Token) -> Result<Value, RuntimeError>{
        match &token.token_type {
            TokenType::Identifier(name) => {
                if let Some(v) = self.vars.get(name) {
                    return Ok(v.clone());
                }else {
                    return Err(RuntimeError::new(token, "Undefined variable [TODO: add var name]"))
                }
            }
            _ => panic!("This shouldn't be here (parser error)")
        }
    }
}
