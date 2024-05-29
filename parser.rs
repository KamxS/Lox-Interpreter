use core::{fmt, panic};
use crate::{error::error, lexer::{Token, TokenType}};

pub enum Expr {
    Literal(Token), 
    Variable(Token),
    Unary(Token, Box<Expr>),
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>)
}
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Unary(t,expr) => write!(f, "({:?}, {})", t.token_type, expr),
            Expr::Binary(expr_l, t, expr_r) => write!(f, "({:?}, {}, {})", t.token_type, expr_l, expr_r),
            Expr::Grouping(expr) => write!(f, "(group, {})", expr),
            Expr::Literal(t) => write!(f, "{:?}", t.token_type),
            Expr::Variable(variable) => write!(f, "{:?}", variable)
        }
    }
}

pub enum Stmt {
    Print(Expr),
    Expression(Expr),
    Var(Token, Option<Expr>)
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
            statements.push(self.declaration());
        }
        statements
    }

    fn declaration(&mut self) -> Stmt {
        if self.check_next(&[TokenType::Var]) {
            self.advance();
            return self.var_declaration();
        }else {
            return self.statement();
        }
    }

    // varDecl → "var" IDENTIFIER ( "=" expression )? ";" ;
    fn var_declaration(&mut self) -> Stmt {
        let token = self.advance();
        if let Some(t) = token {
            match t.token_type {
                TokenType::Identifier(_) => {
                    let mut expr = None;
                    if self.check_next(&[TokenType::Equal]) {
                        self.advance();
                        expr = Some(self.expression());
                    }
                    self.expect_token(TokenType::Semicolon, "Expect ';' after value.");
                    return Stmt::Var(t, expr);
                }
                _ => panic!("Expected an identifier")
            }
        }else {
            //error(token, "Expected an identifier");
            panic!("Expected an identifier");
        }
    }
    
    fn statement(&mut self) -> Stmt {
        if self.check_next(&[TokenType::Print]) {
            self.advance();
            return self.print_statement();
        }else {
            return self.expr_statement();
        }
    }

    fn print_statement(&mut self) -> Stmt{
        let expr = self.expression();
        self.expect_token(TokenType::Semicolon, "Expect ';' after value.");
        Stmt::Print(expr)
    }
    fn expr_statement(&mut self) -> Stmt{
        let expr = self.expression();
        self.expect_token(TokenType::Semicolon, "Expect ';' after expression.");
        Stmt::Expression(expr)
    }

    fn expression(&mut self) -> Expr{
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while self.check_next(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.comparison()));
        }
        return expr;
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while self.check_next(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.term()));
        }
        return expr;
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while self.check_next(&[TokenType::Minus, TokenType::Plus]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.factor()));
        }
        return expr;
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while self.check_next(&[TokenType::Slash, TokenType::Star]) {
            let token = self.advance().unwrap();
            expr = Expr::Binary(Box::new(expr), token, Box::new(self.unary()));
        }
        return expr;
    }

    fn unary(&mut self) -> Expr {
        if self.check_next(&[TokenType::Bang, TokenType::Minus]) {
            let token = self.advance().unwrap();
            return Expr::Unary(token, Box::new(self.primary()));
        }
        self.primary()
    }

    /*
    fn ternary(&mut self) -> Expr {
        let mut expr = self.primary();
        while self.check_next(&[TokenType::QuestionMark]) {
            let token = self.advance().unwrap();
            if self.check_next(&[TokenType::cos]) {
                //expr = Expr::Ternary(Box::new(expr), token, Box::new(self.unary()), token2, Box);
            }else {
                panic!("TODO: Invalid Ternary operator syntax");
            }
        }
        return expr;
    }
    */

    fn primary(&mut self) -> Expr {
        if let Some(token) = self.advance() {
            if matches!(token.token_type, TokenType::Number(_) | TokenType::String(_) | TokenType::True | TokenType::False | TokenType::Nil) {
                return Expr::Literal(token);
            }else if matches!(token.token_type, TokenType::LeftParen) {
                let expr = self.expression();
                self.expect_token(TokenType::RightParen,"Expect ')' after expression.");
                return Expr::Grouping(Box::new(expr));
            }else if matches!(token.token_type, TokenType::Identifier(_)) {
                return Expr::Variable(token);
            }else {
                panic!("TODO: Invalid literal: {:?}", token.token_type)
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

    fn expect_token(&mut self, typ: TokenType, msg: &str) {
        let token = self.advance().expect(msg);
        if !matches!(token.token_type,typ) {
            panic!("{}", msg);
        }
    }
}
