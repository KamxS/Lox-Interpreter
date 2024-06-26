use core::{fmt, panic};
use crate::lexer::{Token, TokenType};

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Token), 
    Variable(Token),
    Unary(Token, Box<Expr>),
    Assign(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Call(Box<Expr>, Token, Vec<Expr>),
    Logical(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>)
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Unary(t,expr) => write!(f, "({:?}, {})", t.token_type, expr),
            Expr::Binary(expr_l, t, expr_r) => write!(f, "({:?}, {}, {})", t.token_type, expr_l, expr_r),
            Expr::Logical(expr_l, t, expr_r) => write!(f, "({:?}, {}, {})", t.token_type, expr_l, expr_r),
            Expr::Grouping(expr) => write!(f, "(group, {})", expr),
            Expr::Literal(t) => write!(f, "{:?}", t.token_type),
            Expr::Variable(variable) => write!(f, "{:?}", variable),
            Expr::Assign(var, expr) => write!(f, "{:?}={}", var, expr),
            Expr::Call(call, _, args) => {
                write!(f, "{}(", call)?;
                for arg in args.iter() {
                    write!(f, "{}, ", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    Block(Vec<Stmt>),
    Var(Token, Option<Expr>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Func(Token, Vec<Token>, Vec<Stmt>),
    Return(Token, Option<Expr>) 
}

#[derive(Debug, Clone)]
struct ParserError {
    token: Token,
    message: String
}
impl ParserError {
    fn new(token: &Token, message: &'static str) -> Self {
        Self { 
            token:token.clone(), 
            message: String::from(message)
        }
    }
    fn new_string(token: &Token, message: String) -> Self {
        Self { 
            token:token.clone(), 
            message
        }
    }
}
impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.token.token_type == TokenType::Eof {
            write!(f,"[line {}] Error at the end: {}", self.token.line, self.message)?
        } else {
            write!(f,"[line {}] Error at '{}': {}", self.token.line, self.token.token_type, self.message)?
        }
        Ok(())
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}
impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {tokens, current: 0}
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements:Vec<Stmt> = vec!();
        while !matches!(self.tokens[self.current].token_type,TokenType::Eof) {
            if let Some(stmt) = self.declaration() {
                statements.push(stmt);
            }
        }
        statements
    }

    fn declaration(&mut self) -> Option<Stmt> {
        let stmt;
        if self.check_next(&[TokenType::Var]) {
            self.advance();
            stmt = self.var_declaration();
        }else if self.check_next(&[TokenType::Fun]) {
            self.advance();
            stmt = self.fun_stmt("function");
        }else {
            stmt = self.statement();
        }
        match stmt {
            Ok(v) => return Some(v),
            Err(err) => {
                eprintln!("{}", err);
                return None;
            }
        }
    }

    fn fun_stmt(&mut self, kind: &'static str) -> Result<Stmt, ParserError> {
        let fun_name = self.advance().unwrap();
        if matches!(fun_name.token_type, TokenType::Id(_)) {
            self.expect_token(TokenType::LeftParen,"Expect '('")?;
            let mut parameters = vec!();
            if !self.check_next(&[TokenType::RightParen]) {
                loop {
                    let par = self.advance();
                    if let Some(id) = par {
                        if matches!(id.token_type, TokenType::Id(_)) {
                            parameters.push(id);
                        }else {
                            return Err(ParserError::new(&self.advance().unwrap(), "Expect parameter name"));
                        }
                    }else {
                        return Err(ParserError::new(&self.advance().unwrap(), "Expect parameter name"));
                    }
                    if !self.check_next(&[TokenType::Comma]) {
                        break;
                    }
                }
            }
            self.expect_token(TokenType::RightParen, "Expect ')' after parameters.")?;
            if parameters.len()>=255 {
                eprintln!("{}", ParserError::new(&fun_name,"Can't have more than 255 parameters."));
            }
            self.expect_token(TokenType::LeftBrace, "Expect '{' before body.")?;
            return Ok(Stmt::Func(fun_name, parameters, self.block()?));
        }else {
            return Err(ParserError::new_string(&self.advance().unwrap(), format!("Expect {} name.", kind)))
        }
    }

    fn return_stmt(&mut self) -> Result<Stmt, ParserError> {
        let token = self.advance().unwrap();
        let mut expr = None;
        if !self.check_next(&[TokenType::Semicolon]) {
            expr = Some(self.expression()?);
        }
        self.expect_token(TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::Return(token, expr))
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let token = self.advance();
        if let Some(t) = token {
            match t.token_type {
                TokenType::Id(_) => {
                    let mut expr = None;
                    if self.check_next(&[TokenType::Equal]) {
                        self.advance();
                        expr = Some(self.expression()?);
                    }
                    self.expect_token(TokenType::Semicolon, "Expect ';' after value.")?;
                    return Ok(Stmt::Var(t, expr));
                }
                _ => return Err(ParserError::new(&t, "Expected variable name"))
            }
        }
        panic!("TODO");
    }
    
    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.check_next(&[TokenType::Print]) {
            self.advance();
            return self.print_statement();
        }else if self.check_next(&[TokenType::LeftBrace]) {
            self.advance();
            return Ok(Stmt::Block(self.block()?));
        }else if self.check_next(&[TokenType::If]) {
            self.advance();
            return self.if_statement();
        }else if self.check_next(&[TokenType::While]) {
            self.advance();
            return self.while_statement();
        }else if self.check_next(&[TokenType::For]) {
            self.advance();
            return self.for_statement();
        }else if self.check_next(&[TokenType::Return]) {
            return self.return_stmt();
        }

        else {
            return self.expr_statement();
        }
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts: Vec<Stmt> = vec!();
        while !self.check_next(&[TokenType::RightBrace]) {
            if let Some(stmt) = self.declaration() {
                stmts.push(stmt);
            }
        }
        self.expect_token(TokenType::RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let initializer_expr;
        if self.check_next(&[TokenType::Var]) {
            self.advance();
            initializer_expr = Some(self.var_declaration()?);
        }else if self.check_next(&[TokenType::Semicolon]) {
            initializer_expr = None;
        } else {
            initializer_expr = Some(self.expr_statement()?);
        }

        let condition_expr;
        if self.check_next(&[TokenType::Semicolon]) {
            condition_expr = None;
        }else {
            condition_expr = Some(self.expression()?);
        }
        self.expect_token(TokenType::Semicolon, "Expect ';' after loop condition.")?;

        let mut increment_expr = None;
        if !self.check_next(&[TokenType::RightParen]) {
            increment_expr = Some(self.expression()?);
        }
        self.expect_token(TokenType::RightParen, "Expect ')' after while condition.")?;
        
        let mut stmt = self.statement()?;
        if let Some(incr) = increment_expr {
            stmt = Stmt::Block(vec!(stmt, Stmt::Expression(incr)));
        }
        stmt = Stmt::While(condition_expr.unwrap_or(Expr::Literal(Token::new(TokenType::True,0))),Box::new(stmt));
        if let Some(init) = initializer_expr {
            stmt = Stmt::Block(vec!(init, stmt));
        }
        Ok(stmt)
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.expect_token(TokenType::RightParen, "Expect ')' after while condition.")?;
        Ok(Stmt::While(condition, Box::new(self.statement()?)))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.expect_token(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.expect_token(TokenType::RightParen, "Expect ')' after if condition.")?;
        let then = self.statement()?;
        let mut else_stmt = None;
        if self.check_next(&[TokenType::Else]) {
            self.advance();
            else_stmt = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::If(condition,Box::new(then),else_stmt))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError>{
        let expr = self.expression()?;
        self.expect_token(TokenType::Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print(expr))
    }
    fn expr_statement(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.expression()?;
        self.expect_token(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, ParserError>{
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.logic_or()?;
        if self.check_next(&[TokenType::Equal]) {
            let equals = self.advance().unwrap();
            if let Expr::Variable(token) = expr {
                return Ok(Expr::Assign(token, Box::new(self.assignment()?)));
            }else {
                return Err(ParserError::new(&equals, "Invalid assignment target."));
            }
        }
        Ok(expr)
    }

    fn logic_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.logic_and()?;
        if self.check_next(&[TokenType::Or]) {
            let token = self.advance().unwrap();
            expr = Expr::Logical(Box::new(expr), token, Box::new(self.logic_and()?));
        }
        return Ok(expr);
    }

    fn logic_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;
        if self.check_next(&[TokenType::And]) {
            let token = self.advance().unwrap();
            expr = Expr::Logical(Box::new(expr), token, Box::new(self.equality()?));
        }
        return Ok(expr);
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while self.check_next(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.comparison()?));
        }
        return Ok(expr);
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while self.check_next(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.term()?));
        }
        return Ok(expr);
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while self.check_next(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.factor()?));
        }
        return Ok(expr);
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while self.check_next(&[TokenType::Slash, TokenType::Star]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.unary()?));
        }
        return Ok(expr);
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if self.check_next(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.advance().unwrap();
            return Ok(Expr::Unary(token, Box::new(self.unary()?)));
        }
        self.call()
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        loop {
            if self.check_next(&[TokenType::LeftParen]) {
                self.advance();
                expr = self.arguments(expr)?
            }else {
                break;
            }
        }
        Ok(expr)
    }

    fn arguments(&mut self, calle: Expr) -> Result<Expr, ParserError> {
        let mut exprs = vec!();
        if !self.check_next(&[TokenType::RightParen]) {
            loop {
                exprs.push(self.expression()?);
                if !self.check_next(&[TokenType::Comma]) {
                    break;
                }
                self.advance();
            } 
        }

        if exprs.len()>=255 {
            eprintln!("{}", ParserError::new(&self.advance().unwrap(), "Can't have more than 255 arguments."));
        }

        Ok(Expr::Call(Box::new(calle), self.advance().unwrap(), exprs))
    }

    fn primary(&mut self) -> Result<Expr,ParserError> {
        if let Some(token) = self.advance() {
            if matches!(token.token_type, TokenType::Number(_) | TokenType::Str(_) | TokenType::True | TokenType::False | TokenType::Nil) {
                return Ok(Expr::Literal(token));
            }else if matches!(token.token_type, TokenType::LeftParen) {
                let expr = self.expression()?;
                self.expect_token(TokenType::RightParen,"Expect ')' after expression.")?;
                return Ok(Expr::Grouping(Box::new(expr)));
            }else if matches!(token.token_type, TokenType::Id(_)) {
                return Ok(Expr::Variable(token));
            }else {
                return Err(ParserError::new(&token, "Expected expression."));
            }
        }else {
            panic!("TODO: No token left");
        }
    }

    fn advance(&mut self) -> Option<Token>{
        self.current+=1;
        if self.current-1<self.tokens.len() {
            return Some(self.tokens[self.current-1].clone());
        }
        None
    }

    fn check_next(&self, tokens: &[TokenType]) -> bool {
        if let Some(t) = self.tokens.get(self.current) {
            for token in tokens {
                if t.token_type == *token {
                    return true;
                }
            }
        }
        false
    }

    fn expect_token(&mut self, typ: TokenType, msg: &'static str) -> Result<(), ParserError> {
        if let Some(t) = self.tokens.get(self.current) {
            if t.token_type == typ {
                self.advance();
                return Ok(());
            }
            return Err(ParserError::new(&t, msg));
        }
        panic!("TODO");
    }
}
