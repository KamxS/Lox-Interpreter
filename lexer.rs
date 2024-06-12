use crate::error::lexer_error;
use core::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    // Literals.
    Id(String), Str(String), Number(f32),
    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    Eof
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
#[derive(Debug, Clone)]
pub struct Token {
    pub token_type:TokenType,
    pub line:u16
}
impl Token {
    pub fn new(token_type: TokenType, line: u16) -> Self {
        Self { token_type, line}
    }
}

pub struct Lexer {
    input: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: u16
}
impl Lexer {
    pub fn new(input: &String) -> Self {
        Self { 
            input: input.clone(), 
            tokens: Vec::new(), 
            start: 0,
            current: 0,
            line: 0,
        }
    }
    pub fn scan(&mut self) {
        while let Some(l) = self.advance() { 
            self.start = self.current;
            match l {
                '{' => self.tokens.push(Token::new(TokenType::LeftBrace, self.line)),
                '}' => self.tokens.push(Token::new(TokenType::RightBrace, self.line)),
                '(' => self.tokens.push(Token::new(TokenType::LeftParen, self.line)),
                ')' => self.tokens.push(Token::new(TokenType::RightParen, self.line)),
                ',' => self.tokens.push(Token::new(TokenType::Comma, self.line)),
                '.' => self.tokens.push(Token::new(TokenType::Dot, self.line)),
                '+' => self.tokens.push(Token::new(TokenType::Plus, self.line)),
                '-' => self.tokens.push(Token::new(TokenType::Minus, self.line)),
                ';' => self.tokens.push(Token::new(TokenType::Semicolon, self.line)),
                '*' => self.tokens.push(Token::new(TokenType::Star, self.line)),
                '!' => {
                    let typ = if self.check('=') {TokenType::BangEqual} else {TokenType::Bang};
                    self.tokens.push(Token::new(typ, self.line))
                }
                '=' => {
                    let typ = if self.check('=') {TokenType::EqualEqual} else {TokenType::Equal};
                    self.tokens.push(Token::new(typ, self.line))
                }
                '>' => {
                    let typ = if self.check('=') {TokenType::GreaterEqual} else {TokenType::Greater};
                    self.tokens.push(Token::new(typ, self.line))
                }
                '<' => {
                    let typ = if self.check('=') {TokenType::LessEqual} else {TokenType::Less};
                    self.tokens.push(Token::new(typ, self.line))
                }
                '/' => {
                    if self.check('/') {
                        // TODO: Comments
                    }else {
                        self.tokens.push(Token::new(TokenType::Slash, self.line));
                    }
                }
                '"' => {
                    while let Some(n) = self.peek() {
                        self.advance();
                        match n {
                            '\n' => self.line+=1,
                            '"' => break,
                            _ => {}
                        }
                    }
                    if self.peek() == None {
                        lexer_error(self.line, "Unterminated string");
                    }
                    let substr = String::from(&self.input[self.start..self.current-1]);
                    self.tokens.push(Token::new(TokenType::Str(substr), self.line));
                }
                '\n' => self.line+=1,
                '\t' => {}
                '\r' => {}
                ' ' => {}
                _ => {
                    if l.is_digit(10){
                        while let Some(n) = self.peek() {
                            if !n.is_digit(10) {
                                match self.peek() {
                                    Some('.') => {}
                                    _ => break
                                }
                            }
                            self.advance();
                        }
                        match self.input[self.start-1..self.current].parse::<f32>() {
                            Ok(v) => self.tokens.push(Token::new(TokenType::Number(v), self.line)),
                            _ => lexer_error(self.line, "Invalid Number")
                        }
                    }else if l.is_alphanumeric() {
                        while let Some(n) = self.peek() {
                            if !n.is_alphanumeric() {
                                break;
                            }
                            self.advance();
                        }
                        match &self.input[self.start-1..self.current] {
                            "and" => self.tokens.push(Token::new(TokenType::And,self.line)),
                            "class" => self.tokens.push(Token::new(TokenType::Class,self.line)),
                            "else" => self.tokens.push(Token::new(TokenType::Else,self.line)),
                            "fun" => self.tokens.push(Token::new(TokenType::Fun,self.line)),
                            "for" => self.tokens.push(Token::new(TokenType::For,self.line)),
                            "if" => self.tokens.push(Token::new(TokenType::If,self.line)),
                            "or" => self.tokens.push(Token::new(TokenType::Or,self.line)),
                            "print" => self.tokens.push(Token::new(TokenType::Print,self.line)),
                            "return" => self.tokens.push(Token::new(TokenType::Return,self.line)),
                            "super" => self.tokens.push(Token::new(TokenType::Super,self.line)),
                            "this" => self.tokens.push(Token::new(TokenType::This,self.line)),
                            "while" => self.tokens.push(Token::new(TokenType::While,self.line)),
                            "var" => self.tokens.push(Token::new(TokenType::Var,self.line)),
                            "true" => self.tokens.push(Token::new(TokenType::True,self.line)),
                            "false" => self.tokens.push(Token::new(TokenType::False,self.line)),
                            "null" => self.tokens.push(Token::new(TokenType::Nil,self.line)),
                            v => self.tokens.push(Token::new(TokenType::Id(String::from(v)), self.line))
                        }
                    }else {
                        lexer_error(self.line, "Unexpected character");
                    }
                }
            }
        }
        self.tokens.push(Token::new(TokenType::Eof, self.line));
    }
    fn advance(&mut self) -> Option<char>{
        self.current+=1;
        self.input.chars().nth(self.current-1)
    }
    fn peek(&self) -> Option<char>{
        self.input.chars().nth(self.current)
    }
    fn check(&mut self, c: char) -> bool {
        match self.input.chars().nth(self.current) {
            Some(l) => {
                self.current+=1;
                c==l
            }
            None => false
        }
    }
    pub fn get_tokens(&self) -> Vec<Token>{
        self.tokens.clone()
    }
}
