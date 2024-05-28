use core::{fmt, panic};
use crate::lexer::{Token, TokenType};

pub enum Expr {
    Literal(Token), 
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
            Expr::Literal(t) => write!(f, "{:?}", t.token_type)
        }
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

    pub fn expression(&mut self) -> Expr{
        self.unary()
    }

    //  ( "!" | "-" ) unary  | primary
    fn unary(&mut self) -> Expr {
        let token = self.peek();
        if let Some(token) = self.peek() {
            if matches!((*token).token_type, TokenType::Bang | TokenType::Minus) {
                self.advance();
                return Expr::Unary(token.clone(), Box::new(self.primary()));
            }else {
                panic!("TODO, unary");
            }
        }else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Expr {
        if let Some(token) = self.advance() {
            if matches!((*token).token_type, TokenType::Number(_) | TokenType::String(_) | TokenType::True | TokenType::False | TokenType::Nil) {
                return Expr::Literal(token.clone());
            }else if matches!((*token).token_type, TokenType::LeftParen) {
                let expr = self.expression();
                let r_paren= self.advance().expect("Expect ')' after expression.");
                assert!(matches!((*r_paren).token_type,TokenType::RightParen));
                return Expr::Grouping(Box::new(expr));
            }else {
                panic!("TODO: Invalid literal")
            }
        }else {
            panic!("TODO: No token left");
        }
    }

    fn advance(&mut self) -> Option<&Token>{
        self.current+=1;
        self.tokens.get(self.current-1)
    }
    fn peek(&self) -> Option<&Token>{
        self.tokens.get(self.current)
    }
}
