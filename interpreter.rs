use core::{fmt, panic};
use std::time::{SystemTime};
use std::collections::HashMap;

use crate::parser::{Expr, Stmt};
use crate::lexer::{Token,TokenType};


fn clock(args: Vec<Value>) -> Value {
    Value::Number(SystemTime::now().elapsed().unwrap().as_secs() as f32)
}

#[derive(PartialEq, Clone)]
pub enum Value {
    String(String), 
    Number(f32), 
    Bool(bool),
    CallableV(Callable),
    Null
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::CallableV(_) => write!(f, "Function_Call()"),
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

#[derive(PartialEq, Clone)]
pub struct Callable {
    arity: u16,
    call_f: fn(Vec<Value>)->Value
}
impl Callable {
    fn call(&self, args: Vec<Value>) -> Value{
        (self.call_f)(args)
    }
}

pub struct Interpreter {
    vars: Vec<HashMap<String, Value>>
}
impl Interpreter {
    pub fn new() -> Self {
        let clock_f = Value::CallableV(Callable{arity: 0, call_f: clock});
        let mut globals = HashMap::new();
        globals.insert(String::from("clock"), clock_f);
        Self{vars: vec!(globals)}
    }
    pub fn interpret(&mut self, statements: &Vec<Stmt>) {
        for stmt in statements.iter() {
            self.execute(stmt);
        }
    }

    fn execute(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Print(_) => self.print_stmt(stmt),
            Stmt::Var(_, _) => self.var_decl_stmt(stmt).unwrap(),
            Stmt::Block(_) => self.block(stmt),
            Stmt::If(_,_,_) => self.if_stmt(stmt).unwrap(),
            Stmt::While(_,_) => self.while_stmt(stmt).unwrap(),
            _ => self.expr_stmt(stmt)
        }
    }

    fn block(&mut self, stmt: &Stmt) {
        if let Stmt::Block(stmts) = stmt{
            self.vars.push(HashMap::new());
            for stmt in stmts.iter() {
                self.execute(stmt)
            }
            self.vars.pop();
        }
    }

    fn if_stmt(&mut self, stmt: &Stmt) -> Result<(),RuntimeError> {
        if let Stmt::If(condition, then, else_stmt) = stmt {
            let cond = self.eval(condition)?;
            if self.get_bool(&cond) {
                self.execute(then);
                return Ok(());
            }else {
                if let Some(s) = else_stmt {
                    self.execute(s);
                }
                return Ok(());
            }
        }
        panic!()
    }

    fn while_stmt(&mut self, stmt: &Stmt) -> Result<(),RuntimeError> {
        if let Stmt::While(condition, stmt) = stmt {
            let mut cond = self.eval(condition)?;
            while(self.get_bool(&cond)) {
                self.execute(stmt);
                cond = self.eval(condition)?;
            }
            return Ok(());
        }
        panic!()
    }

    fn var_decl_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        if let Stmt::Var(token, expr) = stmt {
            let mut val = Value::Null;
            if let Some(e) = expr {
                val = self.eval(e)?;
            }
            self.define(token, val);
            return Ok(());
        }
        panic!()
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
            Expr::Logical(_, _, _) => self.eval_logical(expr),
            Expr::Variable(_) => self.eval_variable(expr),
            Expr::Assign(_,_) => self.eval_assignment(expr),
            Expr::Call(_,_,_) => self.eval_call(expr)
        }
    }

    fn eval_call(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        if let Expr::Call(calle, _, args) = expr {
            let calle_v = self.eval(calle)?;
            if let Value::CallableV(callable) = calle_v {
                let mut args_v = vec!();
                for arg in args.iter() {
                    args_v.push(self.eval(arg)?);
                }
                Ok(callable.call(args_v))
            }else {
                todo!();
            }
        }else {
            panic!()
        }
    }

    fn eval_assignment(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        if let Expr::Assign(name_token, e) = expr {
            let value = self.eval(e)?; 
            self.assign(&name_token, value.clone())?;
            return Ok(value);
        }else {
            panic!()
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

    fn eval_logical(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        if let Expr::Logical(l_expr, t, r_expr) = expr {
            let l_value = self.eval(l_expr)?;

            if matches!(t.token_type, TokenType::Or) {
                if self.get_bool(&l_value) {
                    return Ok(l_value);
                }
            }else {
                if !self.get_bool(&l_value) {
                    return Ok(l_value);
                }
            }
            return self.eval(r_expr);
        }
        panic!()
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
                let ind = self.vars.len()-1;
                self.vars[ind].insert(name.clone(), value);
            }
            _ => panic!("This shouldn't be here (parser error)")
        }
    }

    fn get(&mut self, token: &Token) -> Result<Value, RuntimeError>{
        match &token.token_type {
            TokenType::Identifier(name) => {
                for ind in (0..self.vars.len()).rev() {
                    if let Some(v) = self.vars[ind].get(name) {
                        return Ok(v.clone());
                    }
                }
                return Err(RuntimeError::new(token, "Undefined variable [TODO: add var name]"))
            }
            _ => panic!("This shouldn't be here (parser error)")
        }
    }

    fn assign(&mut self, token: &Token, value: Value) -> Result<(), RuntimeError>{
        match &token.token_type {
            TokenType::Identifier(name) => {
                for ind in (0..self.vars.len()).rev() {
                    if self.vars[ind].contains_key(name) {
                        let v = self.vars[ind].get_mut(name).unwrap();
                        *v = value;
                        return Ok(());
                    }
                }
                return Err(RuntimeError::new(token, "Undefined variable [TODO: add var name]"))
            }
            _ => panic!("This shouldn't be here (parser error)")
        }
    }
}
