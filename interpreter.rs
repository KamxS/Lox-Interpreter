use core::{fmt, panic};
use std::collections::HashMap;

use crate::parser::{Expr, Stmt};
use crate::lexer::{Token,TokenType};

#[derive(Debug, PartialEq)]
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

pub struct Interpreter {
    vars: HashMap<String, Value>
}
impl Interpreter {
    pub fn new() -> Self {
        Self{vars: HashMap::new()}
    }
    pub fn interpret(&self, statements: &Vec<Stmt>) {
        for stmt in statements.iter() {
            self.execute(stmt);
        }
    }

    fn execute(&self, stmt: &Stmt) {
        match stmt {
            Stmt::Print(_) => self.print_stmt(stmt),
            _ => self.expr_stmt(stmt)
        }
    }

    fn print_stmt(&self, stmt: &Stmt) {
        if let Stmt::Print(expr)= stmt{
            println!("{}", self.eval(expr));
        }
    }

    fn expr_stmt(&self, stmt: &Stmt) {
        if let Stmt::Expression(expr)= stmt{
            self.eval(expr);
        }
    }

    fn eval(&self, expr: &Expr) -> Value{
        match expr {
            Expr::Literal(_) => self.eval_literal(expr),
            Expr::Grouping(e) => self.eval(e),
            Expr::Unary(_,_) => self.eval_unary(expr),
            Expr::Binary(_,_,_) => self.eval_binary(expr),
            _ => Value::Null
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

    fn eval_unary(&self, expr: &Expr) -> Value{
        if let Expr::Unary(t, expr) = expr {
            let value = self.eval(expr);
            match t.token_type {
                TokenType::Minus => {
                    return Value::Number(-self.get_num(&value));
                },
                TokenType::Bang=> {
                    return Value::Bool(!self.get_bool(&value));
                },
                _ => panic!("[parser_error] This token can't be a part of unary expression: {:?}", t)
            }
        }
        Value::Null
    }

    fn eval_binary(&self, expr: &Expr) -> Value {
        if let Expr::Binary(l_expr, t, r_expr) = expr {
            let l_value = self.eval(l_expr);
            let r_value = self.eval(r_expr);
            match t.token_type {
                TokenType::Minus => return Value::Number(self.get_num(&l_value)-self.get_num(&r_value)),
                TokenType::Star=> return Value::Number(self.get_num(&l_value)*self.get_num(&r_value)),
                TokenType::Slash => {
                    let r_num = self.get_num(&r_value);
                    if r_num == 0.0 {
                        panic!("Division by 0 is forbidden");
                    }
                    return Value::Number(self.get_num(&l_value)/r_num);
                },
                TokenType::Plus => {
                    if let Value::String(l_str) = l_value {
                        match r_value {
                            Value::String(r_str) => return Value::String(l_str + r_str.as_str()),
                            Value::Number(r_num) => return Value::String(l_str + r_num.to_string().as_str()),
                            _ => panic!("Following type can't be concatinated with a string")
                        }
                    }else {
                        let l_num = self.get_num(&l_value);
                        match r_value {
                            Value::String(r_str) => return Value::String(l_num.to_string() + r_str.as_str()),
                            Value::Number(r_num) => return Value::Number(l_num + r_num),
                            _ => panic!("Expected expected a number or a string")
                        }
                    }
                }
                TokenType::Greater => return Value::Bool(self.get_num(&l_value)>self.get_num(&r_value)),
                TokenType::GreaterEqual => return Value::Bool(self.get_num(&l_value)>=self.get_num(&r_value)),
                TokenType::Less => return Value::Bool(self.get_num(&l_value)<self.get_num(&r_value)),
                TokenType::LessEqual => return Value::Bool(self.get_num(&l_value)<=self.get_num(&r_value)),
                TokenType::EqualEqual => return Value::Bool(self.is_equal(&l_value, &r_value)),
                TokenType::BangEqual => return Value::Bool(!self.is_equal(&l_value, &r_value)),
                _ => return Value::Null
            }
        }
        Value::Null
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
}
